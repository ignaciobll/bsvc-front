from subprocess import Popen, PIPE, STDOUT


# Basic Operations

def sim_write(command):
    # Necesita binario y salto de linea
    p.stdin.write('{}\n'.format(command).encode())
    # Necesario para que la entrada se haga efectiva.
    p.stdin.flush()


def sim_read():
    r = []
    a = ""
    while(not (a == 'Ready!')):
        r.append(a)
        a = p.stdout.readline().decode().strip('\n')
    return r[1:]


# API

def add_breakpoint(addr):
    q = "AddBreakpoint {}".format(addr)
    sim_write(q)
    return sim_read()


def attach_device(addrspace, name, args):
    q = "AttachDevice {} {} {}".format(addrspace, name, args)
    sim_write(q)
    return sim_read()


def clear_statistics():
    q = "ClearStatistics"
    sim_write(q)
    return sim_read()


def detach_device(addrspace, device_index):
    q = "DetachDevice {} {}".format(addrspace, device_index)
    sim_write(q)
    return sim_read()


def delete_breakpoint(addr):
    q = "DeleteBreakpoint {}".format(addr)
    sim_write(q)
    return sim_read()


def fill_memory_block(addrspace, addr, lenght, val):
    q = "FillMemoryBlock {} {} {} {}".format(addrspace, addr, hex(lenght), val)
    sim_write(q)
    return sim_read()


def list_attached_devices(addrspace):
    q = "ListAttachedDevices {}".format(addrspace)
    sim_write(q)
    return sim_read()


def list_breakpoints():
    q = "ListBreakpoints"
    sim_write(q)
    return sim_read()


def list_devices():
    q = "ListDevices"
    sim_write(q)
    return sim_read()


def list_device_script(name):
    q = "ListDeviceScript {}".format(name)
    sim_write(q)
    return sim_read()


def list_execution_trace_record():
    q = "ListExecutionTraceRecord"
    sim_write(q)
    return sim_read()


def list_default_execution_trace_entries():
    q = "ListDefaultExecutionTraceEntries"
    sim_write(q)
    return sim_read()
    sim_write(q)
    return sim_read()


def list_granularity():
    q = "ListGranularity"
    sim_write(q)
    return sim_read()


def list_maximum_address(addrspace):
    q = "ListMaximumAddress {}".format(addrspace)
    sim_write(q)
    return sim_read()


def list_memory(addrspace=0, addr=0, lenght=80, words_per_line=16):
    q = "ListMemory {} {} {} {}".format(addrspace, hex(addr), hex(lenght), hex(words_per_line))
    sim_write(q)
    return sim_read()


def list_number_of_address_spaces():
    q = "ListNumberOfAddressSpaces"
    sim_write(q)
    return sim_read()


def list_registers():
    q = "ListRegisters"
    sim_write(q)
    return sim_read()


def list_register_value(register_name):
    q = "ListRegisterValue {}".format(register_name)
    sim_write(q)
    return sim_read()


def list_register_description(register_name):
    q = "ListRegisterDescription {}".format(register_name)
    sim_write(q)
    return sim_read()


def list_statistics():
    q = "ListStatistics"
    sim_write(q)
    return sim_read()


def load_program(filename, addrspace=0):
    q = "LoadProgram {} {{{}}}".format(addrspace, filename)
    sim_write(q)
    return sim_read()


def program_counter_value():
    q = "ProgramCounterValue"
    sim_write(q)
    return sim_read()


def reset():
    q = "Reset"
    sim_write(q)
    return sim_read()


def run():
    # q = "Run {the-stop-program}"
    print("Use step(n) instead")


def set_memory(addrspace, addr, value):
        q = "SetMemory {} {}".format(addrspace, addr, value)
        sim_write(q)
        return sim_read()


def set_register(register_name, value):
    q = "SetRegister {} {}".format(register_name, value)
    sim_write(q)
    return sim_read()


def step(number=1):
    q = "Step {}".format(number)
    sim_write(q)
    return sim_read()


p = Popen('sim68000', stdin=PIPE, stdout=PIPE, stderr=STDOUT)
bsvc_name = sim_read()
sim_write("ListRegisters")
sim_read()


def setup_practica():
    attach_device('0', 'RAM', '{BaseAddress = 0 Size = 8000}')
    attach_device('0', 'M68681', '{BaseAddress = effc00 OffsetToFirstRegister = 1 OffsetBetweenRegisters = 2 InterruptLevel = 4 PortAStandardInputOutputFlag = 0 PortBStandardInputOutputFlag = 0 PortACommand = xterm -T "M68681 Línea A" -132 -fn fixed -e xtermpipe PortBCommand = xterm- T "M68681 Línea B" -132 -fn fixed -e xtermpipe }')

