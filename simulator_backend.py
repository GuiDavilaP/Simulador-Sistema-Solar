#!/usr/bin/env python3
"""
Simulador backend que roda como processo independente
Comunicação via stdin/stdout usando JSON
"""

import json
import sys
from physics.simulator import SolarSystemSimulator
from physics.bodies import CelestialBody

class SimulatorBackend:
    def __init__(self):
        self.simulator = SolarSystemSimulator()
        self._initialize_solar_system()
        
    def _initialize_solar_system(self):
        """Inicializa o sistema solar com todos os planetas"""
        # Sol (central)
        sun = CelestialBody(
            name="Sol",
            mass=1.989e30,
            position=[0, 0],
            velocity=[0, 0],
            radius=40,
            color=(255, 220, 0)
        )
        
        # Mercúrio
        mercury = CelestialBody(
            name="Mercurio",
            mass=3.285e23,
            position=[5.79e10, 0],
            velocity=[0, 47.36e3],
            radius=6,
            color=(169, 169, 169)
        )

        # Vênus
        venus = CelestialBody(
            name="Venus",
            mass=4.867e24,
            position=[1.082e11, 0],
            velocity=[0, 35.02e3],
            radius=9,
            color=(255, 198, 73)
        )

        # Terra
        earth = CelestialBody(
            name="Terra",
            mass=5.972e24,
            position=[1.496e11, 0],
            velocity=[0, 29.8e3],
            radius=10,
            color=(100, 149, 237)
        )
        
        # Marte
        mars = CelestialBody(
            name="Marte",
            mass=6.39e23,
            position=[2.279e11, 0],
            velocity=[0, 24.1e3],
            radius=8,
            color=(205, 92, 92)
        )
        
        # Júpiter
        jupiter = CelestialBody(
            name="Jupiter",
            mass=1.898e27,
            position=[7.786e11, 0],
            velocity=[0, 13.1e3],
            radius=25,
            color=(255, 165, 0)
        )

        # Saturno
        saturn = CelestialBody(
            name="Saturno",
            mass=5.683e26,
            position=[1.434e12, 0],
            velocity=[0, 9.7e3],
            radius=22,
            color=(238, 232, 205)
        )

        # Urano
        uranus = CelestialBody(
            name="Urano",
            mass=8.681e25,
            position=[2.871e12, 0],
            velocity=[0, 6.8e3],
            radius=16,
            color=(173, 216, 230)
        )

        # Netuno
        neptune = CelestialBody(
            name="Netuno",
            mass=1.024e26,
            position=[4.495e12, 0],
            velocity=[0, 5.4e3],
            radius=15,
            color=(0, 0, 128)
        )

        # Adicionar todos os corpos ao simulador
        bodies = [sun, mercury, venus, earth, mars, jupiter, saturn, uranus, neptune]
        for body in bodies:
            self.simulator.add_body(body)
    
    def handle_command(self, command_data):
        """Processa comandos recebidos da interface"""
        command = command_data.get('command')
        
        if command == 'update':
            # Atualizar simulação
            dt = command_data.get('dt', 0.016)  # Default 60fps
            if not command_data.get('paused', False):
                self.simulator.update(dt)
            return self._get_state_response()
            
        elif command == 'set_time_scale':
            # Alterar velocidade da simulação
            multiplier = command_data.get('multiplier', 1.0)
            self.simulator.time_scale = 86400 * 10 * multiplier
            return {'status': 'ok'}
            
        elif command == 'add_body':
            # Adicionar novo corpo
            body_data = command_data.get('body')
            new_body = CelestialBody(
                name=body_data['name'],
                mass=body_data['mass'],
                position=body_data['position'],
                velocity=body_data['velocity'],
                radius=body_data['radius'],
                color=tuple(body_data['color'])
            )
            self.simulator.add_body(new_body)
            return {'status': 'ok'}
            
        elif command == 'reset':
            # Resetar simulação
            self.simulator = SolarSystemSimulator()
            self._initialize_solar_system()
            return self._get_state_response()
            
        elif command == 'get_state':
            # Retornar estado atual
            return self._get_state_response()
            
        else:
            return {'error': f'Unknown command: {command}'}
    
    def _get_state_response(self):
        """Retorna o estado atual do sistema"""
        # Converter corpos para formato JSON-serializável
        bodies_data = []
        for body in self.simulator.bodies:
            bodies_data.append({
                'name': body.name,
                'position': body.position,
                'radius': body.radius,
                'color': body.color
            })
        
        # Obter estatísticas
        stats = self.simulator.get_system_stats()
        
        return {
            'bodies': bodies_data,
            'stats': stats
        }
    
    def run(self):
        """Loop principal do backend"""
        try:
            # Enviar estado inicial
            initial_state = self._get_state_response()
            print(json.dumps(initial_state), flush=True)
            
            # Loop de comandos
            for line in sys.stdin:
                try:
                    command_data = json.loads(line.strip())
                    response = self.handle_command(command_data)
                    print(json.dumps(response), flush=True)
                except json.JSONDecodeError:
                    error_response = {'error': 'Invalid JSON'}
                    print(json.dumps(error_response), flush=True)
                except Exception as e:
                    error_response = {'error': str(e)}
                    print(json.dumps(error_response), flush=True)
                    
        except KeyboardInterrupt:
            pass
        except Exception as e:
            error_response = {'error': f'Backend error: {str(e)}'}
            print(json.dumps(error_response), flush=True)

if __name__ == '__main__':
    backend = SimulatorBackend()
    backend.run()