#!/usr/bin/env python3
"""
Simulador backend que roda como processo independente
Comunicação via stdin/stdout usando JSON
"""

import json
import sys

from physics.simulator import (
    Simulator,
    DEFAULT_TIME_SCALE
)
from physics.vectors import Vec2
from physics.celestial_body import CelestialBody


class SimulatorBackend:
    def __init__(self):
        self.simulator = Simulator()
        self._initialize_solar_system()
        
    def _initialize_solar_system(self):
        """Inicializa simulador com os corpos do sistema solar"""
        self.simulator.add_bodies(
            [
                CelestialBody(
                    name="Sol",
                    mass=1.989e30,
                    position=Vec2(0, 0),
                    velocity=Vec2(0, 0),
                    radius=40,
                    color=(255, 220, 0)
                ),
                CelestialBody(
                    name="Mercúrio",
                    mass=3.285e23,
                    position=Vec2(5.791e10, 0),
                    velocity=Vec2(0, 47.87e3),
                    radius=6,
                    color=(169, 169, 169)
                ),
                CelestialBody(
                    name="Vênus",
                    mass=4.867e24,
                    position=Vec2(1.082e11, 0),
                    velocity=Vec2(0, 35.02e3),
                    radius=9,
                    color=(255, 198, 73)
                ),
                CelestialBody(
                    name="Terra",
                    mass=5.972e24,
                    position=Vec2(1.496e11, 0),
                    velocity= Vec2(0, 29.78e3),
                    radius=10,
                    color=(100, 149, 237)
                ),
                CelestialBody(
                    name="Marte",
                    mass=6.39e23,
                    position=Vec2(2.279e11, 0),
                    velocity=Vec2(0, 24.077e3),
                    radius=8,
                    color=(205, 92, 92)
                ),
                CelestialBody(
                    name="Júpiter",
                    mass=1.898e27,
                    position=Vec2(7.785e11, 0),
                    velocity=Vec2(0, 13.07e3),
                    radius=25,
                    color=(255, 165, 0)
                ),
                CelestialBody(
                    name="Saturno",
                    mass=5.683e26,
                    position=Vec2(1.429e12, 0),
                    velocity=Vec2(0, 9.69e3),
                    radius=22,
                    color=(238, 232, 205)
                ),
                CelestialBody(
                    name="Urano",
                    mass=8.681e25,
                    position=Vec2(2.871e12, 0),
                    velocity=Vec2(0, 6.81e3),
                    radius=16,
                    color=(173, 216, 230)
                ),
                CelestialBody(
                    name="Netuno",
                    mass=1.024e26,
                    position=Vec2(4.495e12, 0),
                    velocity=Vec2(0, 5.43e3),
                    radius=15,
                    color=(0, 0, 128)
                )
            ]
        )
    
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
            if multiplier <= 0:
                return {'error': 'Time scale must be positive'}
            self.simulator.time_scale = DEFAULT_TIME_SCALE * multiplier
            return {'status': 'ok'}
            
        elif command == 'add_body':
            # Adicionar novo corpo
            body_data = command_data.get('body')
            if not body_data:
                return {'error': 'Body data is required'}
            
            required_fields = ['name', 'mass', 'position', 'radius', 'velocity', 'color']
            for field in required_fields:
                if field not in body_data:
                    return {'error': f'Missing required field: {field}'}
                
            position = body_data['position']
            if not isinstance(position, list) or len(position) != 2:
                return {'error': 'Position must be a list of [x, y]'}
            position = Vec2(position[0], position[1])

            velocity = body_data['velocity']
            if not isinstance(velocity, list) or len(velocity) != 2:
                return {'error': 'Velocity must be a list of [vx, vy]'}
            velocity = Vec2(velocity[0], velocity[1])
            
            try:
                new_body = CelestialBody(
                    name=str(body_data['name']),
                    mass=float(body_data['mass']),
                    radius=float(body_data['radius']),
                    position=position,
                    velocity=velocity,
                    color=tuple(body_data['color'])
                )
                self.simulator.add_body(new_body)
            except Exception as e:
                return {'error': str(e)}
            return {'status': 'ok'}
            
        elif command == 'reset':
            # Resetar simulação
            self.simulator.reset()
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
        for body in self.simulator.get_bodies():
            bodies_data.append({
                'name': body.name,
                'radius': body.radius,
                'position': [body.position.x, body.position.y],
                'color': list(body.color)
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