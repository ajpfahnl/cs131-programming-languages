"""
From the TA code repo:

Note that this piece of code is (of course) only a hint
you are not required to use it
neither do you have to use any of the methods mentioned here
The code comes from
https://asyncio.readthedocs.io/en/latest/tcp_echo.html
To run:
1. start the echo_server.py first in a terminal
2. start the echo_client.py in another terminal
3. follow print-back instructions on client side until you quit
"""

import asyncio
import argparse
import sys
from server import port_dict


class Client:
    def __init__(self, server_name, ip='127.0.0.1', name='client', message_max_length=1e6):
        """
        127.0.0.1 is the localhost
        port could be any port
        """
        self.ip = ip
        try:
            self.port = port_dict[server_name]
        except KeyError:
            print(f"Error: Invalid server name: {server_name}.", file=sys.stderr)
            sys.exit(1)
        self.name = name
        self.message_max_length = int(message_max_length)

    async def tcp_echo_client(self, message):
        """
        on client side send the message for echo
        """
        reader, writer = await asyncio.open_connection(self.ip, self.port)
        print(f'{self.name} send: {message!r}')
        writer.write(message.encode())

        data = await reader.read(self.message_max_length)
        print(f'{self.name} received: {data.decode()!r}')

        print('close the socket')
        # The following lines closes the stream properly
        # If there is any warning, it's due to a bug o Python 3.8: https://bugs.python.org/issue38529
        # Please ignore it
        writer.close()

    def run_until_quit(self):
        # start the loop
        while True:
            # collect the message to send
            message = input("Please input the next message to send: ")
            if message in ['quit', 'exit', ':q', 'exit;', 'quit;', 'exit()', '(exit)']:
                break
            else:
                message += "\n"
                asyncio.run(self.tcp_echo_client(message))


if __name__ == '__main__':
    parser = argparse.ArgumentParser('CS131 project client')
    parser.add_argument('server_name', type=str,
                        help="Required server name to connect to. Either 'Riley', 'Jaquez', 'Juzang', 'Campbell', or 'Bernard'")
    args = parser.parse_args()
    client = Client(args.server_name)
    client.run_until_quit()