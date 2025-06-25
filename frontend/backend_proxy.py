"""
Camada de abstração para o backend do simulador
Permite comunicação via JSON e subprocessos
"""

import json
import subprocess
import threading
import queue


class BackendProxy:
    def __init__(self, backend_path):
        self.backend_path = backend_path
        self.process = None
        self.reader_thread = None
        self.response_queue = queue.Queue()
        self.running = False

        if backend_path.endswith('.py'):
            self.backend_cmd = ['.venv/bin/python3', backend_path]
        else: 
            # Supondo que seja um binário Haskell
            if (not backend_path.startswith('./') and 
                not backend_path.startswith('/')):
                backend_path = './' + backend_path
            self.backend_cmd = [backend_path]
        
        # Estado atual do simulador
        self.current_state = {
            'bodies': [],
            'stats': {
                'total_bodies': 0,
                'collisions': 0
            }
        }
        
    def start(self):
        """Inicia o processo backend"""
        try:
            self.process = subprocess.Popen(
                self.backend_cmd,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                bufsize=1  # Line buffered
            )
            self.running = True
            
            # Thread para ler respostas do backend
            self.reader_thread = threading.Thread(target=self._read_responses)
            self.reader_thread.daemon = True
            self.reader_thread.start()
            
            # Aguardar estado inicial
            initial_response = self._wait_for_response(timeout=5.0)
            if initial_response:
                self.current_state = initial_response
            return True
        
        except Exception as e:
            print(f"Erro ao iniciar backend: {e}")
            return False
    
    def stop(self):
        """Para o processo backend"""
        self.running = False
        if self.process:
            self.process.terminate()
            self.process.wait()
            self.process = None

    def update(self, dt, paused=False):
        """Atualiza a simulação"""
        command = {
            'command': 'update',
            'dt': dt,
            'paused': paused
        }
        print(f"Enviando comando de atualização: {command}", file=open('debug.log', 'a'))
        if self._send_command(command):
            response = self._wait_for_response()
            print(f"Resposta recebida: {response}", file=open('debug.log', 'a'))
            if response and 'bodies' in response:
                self.current_state = response
                return True
        return False
    
    def set_time_scale(self, multiplier):
        """Define a escala de tempo da simulação"""
        command = {
            'command': 'set_time_scale',
            'multiplier': multiplier
        }
        return self._send_command(command)
    
    def add_body(self, name, mass, position, velocity, radius, color):
        """Adiciona um novo corpo ao sistema"""
        command = {
            'command': 'add_body',
            'body': {
                'name': str(name),
                'mass': float(mass),
                'radius': float(radius),
                'position': list(position),
                'velocity': list(velocity),
                'color': list(color)
            }
        }
        return self._send_command(command)
    
    def reset(self):
        """Reseta a simulação"""
        command = {'command': 'reset'}
        if self._send_command(command):
            response = self._wait_for_response()
            if response and 'bodies' in response:
                self.current_state = response
                return True
        return False
    
    def get_bodies(self):
        """Retorna a lista atual de corpos"""
        return self.current_state.get('bodies', [])
    
    def get_stats(self):
        """Retorna estatísticas do sistema"""
        return self.current_state.get('stats', {})
    
    def is_alive(self):
        """Verifica se o backend ainda está rodando"""
        return self.process and self.process.poll() is None
    
    def _read_responses(self):
        """Thread para ler respostas do backend"""
        while self.running and self.process:
            try:
                line = self.process.stdout.readline()
                if not line:
                    break
                response = json.loads(line.strip())
                self.response_queue.put(response)
            except json.JSONDecodeError:
                continue
            except Exception as e:
                if self.running:
                    print(f"Erro ao ler resposta: {e}")
                break
    
    def _send_command(self, command_data):
        """Envia comando para o backend"""
        if not self.process:
            return False
        try:
            command_json = json.dumps(command_data) + '\n'
            self.process.stdin.write(command_json)
            self.process.stdin.flush()
            return True
        except Exception as e:
            print(f"Erro ao enviar comando: {e}")
            return False
    
    def _wait_for_response(self, timeout=1.0):
        """Aguarda resposta do backend"""
        try:
            return self.response_queue.get(timeout=timeout)
        except queue.Empty:
            return None
