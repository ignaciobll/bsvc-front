import uifr
import websockets
import asyncio
import json


regvalue = 1


def list_registers():
    rs = []
    rs_clean = list(map(lambda x: x.split('='), uifr.list_registers()))
    for r in rs_clean:
        (name, value) = r
        rs.append({'name': name.strip(), 'value': value.strip()})
    return rs


async def repl(websocket, path):
    command = await websocket.recv()
    print("[RECV]\t{}".format(command))
    if command == "registers":
        await registers(websocket, path)
    elif "memory" in command:
        await list_memory(websocket, path, command)


async def list_memory(websocket, path, command):
    addr = int(command.split(" ")[1])
    addr = addr - (addr % 16)
    # Defaults: length 80 (4*16) words_per_line 16
    memory = uifr.list_memory(addr=addr)
    clean_mem = list(map(lambda x: x.split(" "), memory))  # Individual
    print('\t[RESP]\t{:.30}...'.format(json.dumps(clean_mem)))
    await websocket.send(json.dumps(clean_mem))


async def registers(websocket, path):
    global regvalue
    regvalue += 1
    rs = list_registers()
    uifr.set_register("D1", regvalue)
    print('\t[RESP]\t{:.30}...'.format(json.dumps(rs)))
    await websocket.send(json.dumps(rs))


uifr.setup_practica()

start_server = websockets.serve(repl, 'localhost', 8765)

asyncio.get_event_loop().run_until_complete(start_server)
asyncio.get_event_loop().run_forever()
