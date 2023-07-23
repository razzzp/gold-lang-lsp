

import socket, sys, json


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
initialized_notif_req = {
    "jsonrpc": "2.0",
	"method": "initialized",
	"params": {}
}
shutdown_req = {
    "jsonrpc": "2.0",
	"method": "shutdown",
	"params": None
}
payload = make_json_rpc(initialize_req)

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((TCP_IP, TCP_PORT))
s.send(bytes(payload,'utf-8'))
while 1:
    data =''
    data = s.recv(BUFFER_SIZE)
    if data != '':
        break
print('received data: ', data)
payload = make_json_rpc(initialized_notif_req)
s.send(bytes(payload,'utf-8'))
payload = make_json_rpc(shutdown_req)
s.send(bytes(payload,'utf-8'))
s.close()