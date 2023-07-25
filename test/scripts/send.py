

import socket, sys, json
import time

TCP_IP = 'localhost'
TCP_PORT = 5001
BUFFER_SIZE = 1024 

# if len(sys.argv) < 2:
#     print('please provide a file')
#     sys.exit(1)
    
# file_to_send = sys.argv[1]
# with open(file_to_send, 'r') as f:
#     payload = f.read()

def make_json_rpc(obj):
    json_str = json.dumps(obj)
    return  "Content-Length: {}\r\n\r\n".format(len(json_str)) + json_str


initialize_req = {
    "jsonrpc": "2.0",
    "id": 1,
	"method": "initialize",
	"params": {
		"processId": None,
		"rootUri": None,
		"capabilities": {}
	}
}
initialized_notif = {
    "jsonrpc": "2.0",
	"method": "initialized",
	"params": {}
}
shutdown_req = {
    "jsonrpc": "2.0",
    "id": 2,
    "method": "shutdown",
	"params": None
}
exit_notif = {
    "jsonrpc": "2.0",
    "method": "exit",
	"params": None
}
doc_sym_req = {
    "jsonrpc": "2.0",
    "id": 3,
    "method": "textDocument/documentSymbol",
	"params": {
        "uri": "test/aTestClass.god"
    }
}

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((TCP_IP, TCP_PORT))

payload = make_json_rpc(initialize_req)
s.send(bytes(payload,'utf-8'))

data = s.recv(BUFFER_SIZE)
print('received data: ', data)

time.sleep(1)
payload = make_json_rpc(initialized_notif)
s.send(bytes(payload,'utf-8'))

time.sleep(1)
payload = make_json_rpc(doc_sym_req)
s.send(bytes(payload,'utf-8'))

data = s.recv(BUFFER_SIZE)
print('received data: ', data)

time.sleep(1)
payload = make_json_rpc(shutdown_req)
s.send(bytes(payload,'utf-8'))

data = s.recv(BUFFER_SIZE)
print('received data: ', data)

time.sleep(1)
payload = make_json_rpc(exit_notif)
s.send(bytes(payload,'utf-8'))
s.close()