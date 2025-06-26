import math
from typing import List

from .celestial_body import CelestialBody
from .vectors import Vec2
from .physical_constants import (
    DAY_DURATION, 
    EPS, 
    GRAVIT_CONST as G
)

# Escala de tempo padrão para simulação (10 dias durante 1 segundo real)
DEFAULT_TIME_SCALE = 10 * DAY_DURATION 


class Simulator:
    def __init__(self, time_scale: float = DEFAULT_TIME_SCALE):
        self.bodies = {}
        self.time_scale = time_scale
        self.num_collisions = 0

    @property
    def time_scale(self):
        return self._time_scale
    
    @time_scale.setter
    def time_scale(self, value: float):
        try:
            value = float(value)
        except (TypeError, ValueError):
            raise ValueError("Time scale must be a number")
        if value <= EPS:
            raise ValueError("Time scale must be greater than zero")
        self._time_scale = value

    def add_body(self, body: CelestialBody):
        """Adiciona um corpo celeste ao sistema"""
        if not isinstance(body, CelestialBody):
            raise TypeError("body must be an instance of CelestialBody")
        if body.name in self.bodies:
            raise ValueError(f"Body with name '{body.name}' already exists")
        self.bodies[body.name] = body

    def add_bodies(self, bodies: List[CelestialBody]):
        """Adiciona múltiplos corpos celestes ao sistema"""
        if not isinstance(bodies, list):
            raise TypeError("bodies must be a list of CelestialBody instances")
        for body in bodies:
            try:
                self.add_body(body)
            except Exception as e:
                print(f"Warning: {e}")

    def remove_body(self, body_name: str):
        """Remove um corpo celeste do sistema"""
        if not isinstance(body_name, str):
            raise TypeError("body must be an instance of str")
        if body_name not in self.bodies:
            print(f"Warning: No body with name '{body_name}' found")
        else:
            del self.bodies[body_name]

    def remove_bodies(self, body_names: List[str]):
        """Remove múltiplos corpos celestes do sistema"""
        if not isinstance(body_names, list):
            raise TypeError("bodies must be a list of body names")
        for body_name in body_names:
            try:
                self.remove_body(body_name)
            except Exception as e:
                print(f"Warning: {e}")

    def get_bodies(self) -> List[CelestialBody]:
        """Retorna uma lista de todos os corpos celestes no sistema"""
        return list(self.bodies.values())
    
    def get_body(self, name: str) -> CelestialBody:
        """Retorna um corpo celeste pelo nome"""
        if not isinstance(name, str):
            raise TypeError("name must be an instance of str")
        body = self.bodies.get(name)
        if body is None:
            raise ValueError(f"No body found with name '{name}'")
        return body

    def update(self, dt: float):
        """Atualiza o estado do simulador com detecção de colisões"""
        try:
            dt = float(dt)
        except (TypeError, ValueError):
            raise ValueError("dt must be a number")
        if dt < 0:
            raise ValueError("dt cannot be negative")
        if len(self.bodies) < 2:
            return
        
        # Primeiro, atualiza a física
        dt_scaled = dt * self.time_scale
        self._update_physics(dt_scaled)
        
        # Depois, verifica colisões e remove corpos
        self._manage_collisions()

    def get_system_stats(self):
        """Retorna estatísticas do sistema"""
        # total_mass = sum(body.mass for body in self.bodies)
        # total_kinetic_energy = sum(
        #     0.5 * body.mass * (body.velocity.x**2 + body.velocity.y**2) 
        #     for body in self.bodies
        # )
        return {
            # 'total_mass': total_mass,
            # 'total_kinetic_energy': total_kinetic_energy,
            'total_bodies': len(self.bodies),
            'collisions': self.num_collisions
        }
    
    def reset(self):
        """Reseta o sistema, removendo todos os corpos e estatísticas"""
        self.bodies.clear()
        self.num_collisions = 0

    def _manage_collisions(self):
        """Gerencia colisões do sistema"""
        destroyed_bodies = set()

        bodies = self.get_bodies()
        if len(bodies) < 2:
            return
        
        for i, body1 in enumerate(bodies):
            for body2 in bodies[i+1:]:
                if body1.mass == 0 or body2.mass == 0:
                    continue
                if not body1.check_collision(body2):
                    continue
                # Colisão detectada: Determinar qual corpo remover
                if body1.mass < body2.mass:
                    destroyed_bodies.add(body1.name)
                elif body1.mass > body2.mass:
                    destroyed_bodies.add(body2.name)
                else:
                    # Se as massas forem iguais, remover ambos
                    destroyed_bodies.add(body1.name)
                    destroyed_bodies.add(body2.name)
                self.num_collisions += 1

        self.remove_bodies(list(destroyed_bodies))

    def _update_physics(self, dt_scaled: float):
        """Atualiza a física do sistema"""
        if len(self.bodies) < 2:
            return
        
        forces = {key: Vec2(0.0, 0.0) for key in self.bodies.keys()}
        bodies = self.get_bodies()
        
        # Calcula forças entre todos os pares de corpos
        for i, body1 in enumerate(bodies):
            for body2 in bodies[i+1:]:
                # Aplica a 3ª Lei de Newton (ação e reação)
                force = self._compute_gravitational_force(body1, body2)
                forces[body1.name] += force
                forces[body2.name] -= force
        
        # Atualiza velocidades e posições
        for key, body in self.bodies.items():
            if body.mass == 0:
                continue
            # Não aplicar aceleração se força for muito pequena
            f = forces[key]
            if f.length() < EPS:
                continue
            a = f / body.mass # Aceleração = Força / Massa
            self.bodies[key].velocity += a * dt_scaled 
            self.bodies[key].update_position(dt_scaled)

    def _compute_gravitational_force(self, body1, body2):
        """
        Calcula a força gravitacional entre dois corpos.
        Retorna a força aplicada em body1 devido a body2.
        """
        # Calcular vetor distância
        dx = body2.position.x - body1.position.x
        dy = body2.position.y - body1.position.y
        distance_squared = dx**2 + dy**2
        distance = math.sqrt(distance_squared)
        
        # Evitar divisão por zero ou distâncias muito pequenas
        # Usar o maior entre a distância calculada e um mínimo baseado nos raios
        min_distance = (body1.radius + body2.radius) * 1e9  # Converter pixels para metros
        if distance < min_distance:
            distance = min_distance
            distance_squared = distance**2
        
        # Calcular magnitude da força gravitacional
        force_magnitude = G * body1.mass * body2.mass / distance_squared
        
        # Componentes da força (unitário * magnitude)
        if distance > 0:
            force_x = force_magnitude * dx / distance
            force_y = force_magnitude * dy / distance
        else:
            force_x = force_y = 0
        
        return Vec2(force_x, force_y)