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
import logging
import re
import time
import aiohttp
import json

from aiohttp import client

API_KEY = "AIzaSyBPd6y3IZUkFms4hcb8XAycEcWOlcniZbc"

port_dict = {"Riley": 12326, "Jaquez": 12327, "Juzang": 12328, "Campbell": 12329, "Bernard": 12330}
relations = {
    "Riley": ["Jaquez", "Juzang"],
    "Bernard": ["Jaquez", "Juzang", "Campbell"],
    "Juzang": ["Campbell", "Riley", "Bernard"],
    "Jaquez": ["Riley", "Bernard"],
    "Campbell": ["Bernard", "Juzang"]
}

def isnumber(num_str):
    try:
        float(num_str)
    except:
        return False
    return True

class Server:
    def __init__(self, name, ip='127.0.0.1', message_max_length=1e6):
        self.name = name
        self.ip = ip
        
        try:
            self.port = port_dict[name]
        except KeyError:
            print(f"Error: Invalid server name: {name}.", file=sys.stderr)
            sys.exit(1)

        self.message_max_length = int(message_max_length)
        self.neighbors = relations[name]
        self.client_messages = {}

        # logging
        logging.basicConfig(filename=f'{name}.log', level=logging.DEBUG)
    
    def msg_invalid(self, message_orig):
        logging.error("Invalid message")
        return f"? {message_orig}"

    def msg_iamat(self, message):
        if (len(message) != 4):
            return None
        # e.g.: IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1621464827.959498503
        # check valid ISO6709 format and POSIX time
        iso_matches = len(re.findall('^[+-][0-9]+\.[0-9]+[+-][0-9]+\.[0-9]+$', message[2]))
        if (iso_matches != 1) or not (isnumber(message[3])):
            return None
        logging.info("Valid IAMAT")

        time_diff = time.time() - float(message[3])
        time_diff = f"+{time_diff}" if time_diff >= 0 else f"-{time_diff}"

        # e.g. AT Riley +0.263873386 kiwi.cs.ucla.edu +34.068930-118.445127 1621464827.959498503
        message_send = f"AT {self.name} {time_diff} {message[1]} {message[2]} {message[3]}"
        self.client_messages[message[1]] = message_send
        return message_send

    async def msg_whatsat(self, message):
        # e.g.: WHATSAT kiwi.cs.ucla.edu 10 5
        # check validity
        if (len(message) != 4):
            return None
        if (not (isnumber(message[2]) and isnumber(message[3]))):
            return None
        client_name = message[1]
        radius = int(message[2])
        upper_bound = int(message[3])
        if ((radius > 50) or (radius < 0) or (upper_bound > 20) or (upper_bound < 0)):
            return None
        if client_name not in self.client_messages:
            return None
        # query
        logging.info("Valid WHATSAT")
        client_msg = self.client_messages[client_name]
        loc = client_msg.split()[4]
        places_nearby = await self.get_places_nearby(loc, radius, upper_bound)
        message_send = f"{client_msg}\n{places_nearby}\n\n"
        return message_send
        
    async def get_places_nearby(self, location, radius, upper_bound):
        # coordinates for Google Places API
        # separate by comma, remove any "+"
        coords = ','.join(re.findall('[-]?[^+-]+', location))
        url = "https://maps.googleapis.com/maps/api/place/nearbysearch/json"
        params = [("key", API_KEY), ("location", coords), ("radius", radius)]
        logging.info(f"Querying location {coords}")
        async with aiohttp.ClientSession() as session:
            async with session.get(url, params=params) as resp:
                logging.debug(str(resp.url))
                txt = await resp.text()
        places_data = json.loads(txt)
        results = places_data['results']
        logging.info(f"Retrieved {len(results)}")
        places_data["results"] = results[0:upper_bound]
        return json.dumps(places_data).rstrip('\n')

    async def propagate(self, message):
        for neighbor in self.neighbors:
            try:
                reader, writer = reader, writer = await asyncio.open_connection('127.0.0.1', port_dict[neighbor])
            except:
                logging.error(f"Couldn't connect to server {neighbor}")
                continue
            logging.info(f"Propagating from {self.name} to {neighbor}: {message}")
            writer.write(message.encode())
            await writer.drain()
            writer.close()
            await writer.wait_closed()
            logging.info(f"Sent and connection closed")

    def msg_at(self, message):
        '''
        returns message, propagate_bool, write_bool
        '''
        if len(message) != 6:
            return None, False, True
        logging.info("Propagated message received")
        _, origin, time_diff, client_name, loc, posix_time = message
        message_str = " ".join(message)
        if client_name not in self.client_messages:
            logging.info("Populating new client data")
            self.client_messages[client_name] = message_str
            return message_str, True, False
        if (float(posix_time) > float(self.client_messages[client_name].split()[5])):
            logging.info("Updating client data")
            self.client_messages[client_name] = message_str
            return message_str, True, False
        else:
            logging.info("Message already received")
            return "", False, False
        

    async def handle_echo(self, reader, writer):
        """
        on server side
        """
        while not reader.at_eof():
            data = await reader.readline()
            message_orig = data.decode()
            message = message_orig.split()
            if len(message) == 0:
                continue
            message_send = None
            propagate_bool = True
            write_bool = True
            logging.info(f"#### Received message: {message_orig}")

            if message[0] == "IAMAT":
                message_send = self.msg_iamat(message)
            elif message[0] == "WHATSAT":
                message_send = await self.msg_whatsat(message)
            elif message[0] == "AT":
                message_send, propagate_bool, write_bool = self.msg_at(message)

            # check for invalid messages
            if message_send == None:
                message_send = self.msg_invalid(message_orig)
                propagate_bool = False
            
            # propogate
            if propagate_bool:
                await self.propagate(message_send)

            # send message
            if write_bool:
                writer.write(message_send.encode())
                await writer.drain()

        logging.info("close the client socket")
        writer.close()

    async def run_forever(self):
        server = await asyncio.start_server(self.handle_echo, self.ip, self.port)

        # Serve requests until Ctrl+C is pressed
        logging.info(f'server {self.name} serving on {server.sockets[0].getsockname()}')
        async with server:
            await server.serve_forever()
        # Close the server
        server.close()
        logging.info('server closed')


def main():
    parser = argparse.ArgumentParser('CS131 project server')
    parser.add_argument('server_name', type=str,
                        help="Required server name input. Either 'Riley', 'Jaquez', 'Juzang', 'Campbell', or 'Bernard'")
    args = parser.parse_args()

    server = Server(args.server_name)
    try:
        asyncio.run(server.run_forever())
    except KeyboardInterrupt:
        pass


if __name__ == '__main__':
    main()