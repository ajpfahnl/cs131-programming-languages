import java.util.zip.*;

import java.io.*;
// import java.nio.file.*;
// import java.lang.*;

class MultiThreadedGZipCompressor implements Runnable{
    public final static int BLOCK_SIZE = 131072; // 128 KiB
    public final static int DICT_SIZE = 32768;
    private final static int GZIP_MAGIC = 0x8b1f;
    private final static int TRAILER_SIZE = 8;

    public int processes;
    public long totalBytesRead;
    public volatile long blockwrite;
    public InputStream inStream;
    public CRC32 crc = new CRC32();
    public boolean finished;

    public int threadNum;
    public int prevBlockThread;
    public byte[][] threadBuffers;
    public int[] threadBufferLen;
    public byte[][] threadDictBuffers;
    public long[] threadBlock;
    public int lastBlock;
    public boolean[] threadBufferInUse;

    public MultiThreadedGZipCompressor(int processes) {
        this.processes = processes;
        this.totalBytesRead = 0;
        this.blockwrite = 0;
        this.inStream = System.in;
        this.finished = false;

        this.threadNum = 0;
        this.prevBlockThread = -1;
        this.threadBuffers = new byte[processes][BLOCK_SIZE];
        this.threadBufferLen = new int[processes];
        this.threadDictBuffers = new byte[processes][DICT_SIZE];
        this.threadBlock = new long[processes];
        this.threadBufferInUse = new boolean[processes];
        
        for (int i=0; i<processes; i++) {
            this.threadBlock[i] = -1;
            this.threadBufferInUse[i] = false;
        }
    }

    private void writeOut(byte[] b) {
        try {
            System.out.write(b);
        } catch (IOException e) {
            System.err.println("\n\tError: can't write to stdout.");
            System.exit(1);
        }
        if (System.out.checkError()) {
            System.err.println("\n\tError: checkError found an error.");
            System.exit(1);
        }
    }

    private void writeOut(byte[] b, int off, int len) {
        System.out.write(b, off, len);
        if (System.out.checkError()) {
            System.err.println("\n\tError: checkError found an error.");
            System.exit(1);
        }
    }

    public void writeHeader() throws IOException {
        writeOut(new byte[] {
            (byte) GZIP_MAGIC,        // Magic number (short)
            (byte)(GZIP_MAGIC >> 8),  // Magic number (short)
            Deflater.DEFLATED,        // Compression method (CM)
            0,                        // Flags (FLG)
            0,                        // Modification time MTIME (int)
            0,                        // Modification time MTIME (int)
            0,                        // Modification time MTIME (int)
            0,                        // Modification time MTIME (int)Sfil
            0,                        // Extra flags (XFLG)
            0                         // Operating system (OS)
        });
    }


    /*
     * Writes GZIP member trailer to a byte array, starting at a given
     * offset.
      */
    private void writeTrailer(long totalBytes, byte[] buf, int offset) throws IOException {
        writeInt((int)crc.getValue(), buf, offset); // CRC-32 of uncompr. data
        writeInt((int)totalBytes, buf, offset + 4); // Number of uncompr. bytes
    }

    /*
     * Writes integer in Intel byte order to a byte array, starting at a
     * given offset.
     */
    private void writeInt(int i, byte[] buf, int offset) throws IOException {
        writeShort(i & 0xffff, buf, offset);
        writeShort((i >> 16) & 0xffff, buf, offset + 2);
    }

    /*
     * Writes short integer in Intel byte order to a byte array, starting
     * at a given offset
     */
    private void writeShort(int s, byte[] buf, int offset) throws IOException {
        buf[offset] = (byte)(s & 0xff);
        buf[offset + 1] = (byte)((s >> 8) & 0xff);
    }

    public void writeTrailerWrapper() throws FileNotFoundException, IOException {
        byte[] trailerBuf = new byte[TRAILER_SIZE];
        writeTrailer(totalBytesRead, trailerBuf, 0);
        writeOut(trailerBuf);
    }

    synchronized private void setInUse(int i, boolean b) {
        threadBufferInUse[i] = b;
    }

    synchronized private boolean checkInUse(int i) {
        return threadBufferInUse[i];
    }

    public void mainThread() throws FileNotFoundException, IOException {
        // read from stdin into buffers continuously.
        boolean first = true;
        int nBytes = -1;
        int blocksRead = 0;
        byte[] savedLastBuf = new byte[DICT_SIZE];
        boolean reading = true;
        while (reading) {
            // READ
            for (int i=0; i<processes; i++) {
                // only read if thread's buffer is open
                if (threadBufferInUse[i]) {
                    continue;
                }

                /* read into thread i's buffer, then:
                    1. update total bytes read, and set last block
                    2. update thread's associated block number
                    3. update CRC
                    4. if this isn't the first block, copy last part of previous input read 
                       for dictionary
                    5. copy the last part of the current input read to be used for dictionary 
                       of next block
                    6. set thread buffer as "in use"
                   NOTE: if the input is empty, we run this at least once with 0 bytes read
                   and one thread will create an empty compressed block.
                */

                nBytes = inStream.read(threadBuffers[i], 0, BLOCK_SIZE);
                threadBufferLen[i] = nBytes;
                if ((nBytes == -1) && (!first)) {
                    // !first is so that we "read" into a buffer at least once
                    reading = false;
                    break;
                }
                if (nBytes == -1) {
                    // corner case where we read an empty file
                    nBytes = 0;
                    threadBufferLen[i] = 0;
                }
                first = false;

                // 1
                totalBytesRead += nBytes;
                lastBlock = blocksRead;
                
                // 2
                crc.update(threadBuffers[i], 0, nBytes);

                // 3
                threadBlock[i] = blocksRead; 
                blocksRead += 1;

                // 4
                if (threadBlock[i] != 0) {
                    System.arraycopy(savedLastBuf, 0, threadDictBuffers[i], 0, DICT_SIZE);
                }
                
                // 5
                if (nBytes >= DICT_SIZE) {
                    System.arraycopy(threadBuffers[i], nBytes - DICT_SIZE, savedLastBuf, 0, DICT_SIZE);
                }

                // 6
                setInUse(i, true);
                
            }
        }

        // check if each thread is finished
        int numFinished = 0;
        while(numFinished != processes) {
            numFinished = 0;
            for (int i=0; i<processes; i++) {
                if (!threadBufferInUse[i]) {
                    numFinished += 1;
                }
            }
        }
        finished = true;
    }

    public void run() {
        // Compressing logic for each thread

        byte[] cmpBlockBuf = new byte[BLOCK_SIZE * 2];
        // assign thread its own number
        int myThreadNum = -1;
        synchronized(this) {
            myThreadNum = this.threadNum;
            this.threadNum += 1;
        }
        
        Deflater compressor = new Deflater(Deflater.DEFAULT_COMPRESSION, true);

        while (!finished) {
            // see if the block in buffer is ready to be processed
            if (!checkInUse(myThreadNum)) {
                continue;
            }
            
            compressor.reset();

            // prime the deflater with dictionary
            if (threadBlock[myThreadNum] != 0) {
                compressor.setDictionary(threadDictBuffers[myThreadNum]);
            }

            compressor.setInput(threadBuffers[myThreadNum], 0, threadBufferLen[myThreadNum]);

            // Deflate and then write the compressed block out. Not using SYNC_FLUSH here leads to
            // some issues, but using it probably results in less efficient compression. There's probably a better
            // way to deal with this. Also make sure to write in order with other threads.
            int deflatedBytes = compressor.deflate(cmpBlockBuf, 0, cmpBlockBuf.length, Deflater.SYNC_FLUSH);
            if (deflatedBytes > 0) {
                while (blockwrite != threadBlock[myThreadNum]) {}
                writeOut(cmpBlockBuf, 0, deflatedBytes);
                blockwrite += 1;
            }

            // set the thread's buffer as "not in use"
            setInUse(myThreadNum, false);
        }

        // Check if this thread has the last block. If it does, have to clean out the 
        // deflater properly
        if (threadBlock[myThreadNum] == lastBlock) {
            if (!compressor.finished()) {
                compressor.finish();
                while (!compressor.finished()) {
                    int deflatedBytes = compressor.deflate(cmpBlockBuf, 0, cmpBlockBuf.length, Deflater.NO_FLUSH);
                    if (deflatedBytes > 0) {
                        writeOut(cmpBlockBuf, 0, deflatedBytes);
                    }
                }
            }
        }
    }
}

public class Pigzj {
    public static MultiThreadedGZipCompressor cmp;
    public static void main (String[] args) throws FileNotFoundException, IOException {
        int processes = Runtime.getRuntime().availableProcessors();
        int avail_processes = processes;
        
        // Parse -p processes option
        if (args.length > 0) {
            if ((args.length != 2) || (!args[0].equals("-p"))) {
                System.err.println("Error: only `-p processes` where processes is the number of cores to use is allowed.");
                System.exit(1);
            }
            processes = Integer.parseInt(args[1]);
            if (processes > avail_processes) {
                System.err.println("Error: requested more processors (" + processes + ") than available (" + avail_processes +")." );
                System.exit(1);
            }
        }

        cmp = new MultiThreadedGZipCompressor(processes);
        cmp.writeHeader();
        cmp.crc.reset();

        // start threads
        Thread myThreads[] = new Thread[processes];
        for (int i=0; i<processes; i++) {
            myThreads[i] = new Thread(cmp);
            myThreads[i].start();
        }

        // input reading
        cmp.mainThread();

        // join threads
        for (int i=0; i<processes; i++) {
            try {
                myThreads[i].join();
            }
            catch (InterruptedException e) {
                throw new RuntimeException("\n\tError with recombining threads.");
            }
        }

        // write trailer
        cmp.writeTrailerWrapper();
    }
}