import asyncio

async def main():
    server = await asyncio.start_server(handle_connection, host='127.0.0.1', port=12345)
    await server.serve_forever()

async def handle_connection(reader, writer):
    data = await reader.readline()
    name = data.decode()
    greeting = "Hello, " + name
    writer.write(greeting.encode())
    await writer.drain()
    writer.close()

if __name__ == "__main__":
    asyncio.run(main())