import sys
import time
import random
from multiprocessing import Process, Value, Lock, Condition
from collections import deque


class ExtendedCondition:
    def __init__(self, lock):
        self.high_priority_waiters = deque()
        self.normal_priority_waiters = deque()
        self.lock = lock
        self.condition = Condition(lock)

    def __enter__(self):
        self.lock.acquire()
        return self
    
    def __exit__(self, type, value, traceback):
        self.lock.release()

    def wait(self, priority=1):
        waiter = Condition(self.lock)
        if priority == 0:
            self.high_priority_waiters.append(waiter)
        else:
            self.normal_priority_waiters.append(waiter)

        waiter.acquire()
        self.lock.release()
        waiter.wait()
        self.lock.acquire()
        waiter.release()

    def notify(self):
        if self.high_priority_waiters:
            waiter = self.high_priority_waiters.popleft()
            waiter.acquire()
            waiter.notify()
            waiter.release()
        elif self.normal_priority_waiters:
            waiter = self.normal_priority_waiters.popleft()
            waiter.acquire()
            waiter.notify()
            waiter.release()

    def empty(self):
        return not (self.high_priority_waiters or self.normal_priority_waiters)

class RW:
    def __init__(self):
        self.lock = Lock()
        self.readers = Value('i', 0)
        self.writers = Value('i', 0)
        self.can_read = ExtendedCondition(self.lock)
        self.can_write = ExtendedCondition(self.lock)

    def start_read(self):
        with self.can_read:
            while self.writers.value > 0:
                self.can_read.wait()
            self.readers.value += 1

    def end_read(self):
        with self.can_read:
            self.readers.value -= 1
            if self.readers.value == 0 and not self.can_write.empty():
                self.can_write.notify()

    def start_write(self):
        with self.can_write:
            self.writers.value += 1
            while self.readers.value != 0:
                self.can_write.wait(priority=0)

    def end_write(self):
        with self.can_write:
            self.writers.value -= 1
            if not self.can_read.empty():
                self.can_read.notify()
            elif not self.can_write.empty():
                self.can_write.notify()

def process_writer(identifier, synchro):
    synchro.start_write()
    for _ in range(5):
        with open('LectRed_shared', 'a') as file_id:
            txt=' '+str(identifier)
            file_id.write(txt)
            print('Writer', identifier, 'just wrote', txt)
        time.sleep(.1 + random.random())            
    synchro.end_write()

def process_reader(identifier, synchro):
    synchro.start_read()
    position = 0
    result = ''
    while True:
        time.sleep(.1 + random.random())            
        with open('LectRed_shared', 'r') as file_id:
            file_id.seek(position, 0)
            txt = file_id.read(1)
            if len(txt) == 0:
                break
            print('Reader', identifier, 'just read', txt)
            result += txt
            position+=1
    print(str(identifier)+':',result)
    synchro.end_read()
        
if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Usage: %s <Nb reader> <Nb writer> \n" % sys.argv[0])
        sys.exit(1)

    nb_reader = int(sys.argv[1])
    nb_writer = int(sys.argv[2])

    synchro = RW()

    # To initialize the common data
    with open('LectRed_shared', 'w') as file_id:
        file_id.write('')
    
    processes = []
    for id_writer in range(nb_writer):
        writer = Process(target=process_writer, args=(id_writer,synchro))
        writer.start()
        processes.append(writer)

    for id_reader in range(nb_reader):
        reader = Process(target=process_reader, args=(id_reader,synchro))
        reader.start()
        processes.append(reader)

    for process in processes:
        process.join()