import uifr
import websockets
import asyncio
import json
import subprocess
from datetime import datetime

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
    print("[RECV]\t{:.30}...".format(command))
    if command == "registers":
        await registers(websocket, path)
    elif "memory" in command:
        await list_memory(websocket, path, command)
    # elif command == "loadprogram":
    #     await load_program(websocket, path, command)
    elif "step" in command:
        await step(websocket, path, command)
    elif "filecontent" == command[:11]:
        await load_file(websocket, path, command)


async def load_file(websocket, path, command):
    pos = len("filecontent ")
    content = command[pos:]
    file_name = str(datetime.utcnow()).replace(' ', '_').replace('.', '_').replace(':', '-')
    with open('./upload/' + file_name, 'w') as s:
        s.write(content)
    subprocess.run(args=["68kasm",  "{}".format(file_name)], cwd='./upload')
    await load_program(websocket, path, './upload/' + file_name + '.h68')


async def step(websocket, path, command):
    n_steps = int(command.split(" ")[1])
    uifr.step(n_steps)
    await registers(websocket, path)


async def load_program(websocket, path, command):
    program = command
    uifr.load_program(program)
    uifr.reset()
    await registers(websocket, path)


async def list_memory(websocket, path, command):
    addr = int(command.split(" ")[1])
    addr = addr - (addr % 16)
    # Defaults: length 80 (4*16) words_per_line 16
    memory = uifr.list_memory(addr=addr)
    clean_mem = list(map(lambda x: x.split(" "), memory))  # Individual
    print('\t[RESP]\t{:.30}...'.format(json.dumps(clean_mem)))
    await websocket.send(json.dumps(clean_mem))


async def registers(websocket, path):
    rs = list_registers()
    print('\t[RESP]\t{:.30}...'.format(json.dumps(rs)))
    await websocket.send(json.dumps(rs))


uifr.setup_practica()

start_server = websockets.serve(repl, 'localhost', 8765)

asyncio.get_event_loop().run_until_complete(start_server)
asyncio.get_event_loop().run_forever()
