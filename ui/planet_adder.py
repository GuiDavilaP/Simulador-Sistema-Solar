import math
import random

DEFAULT_MASS = 5.972e24  # Massa da Terra como padrão

class PlanetAdder:
    REFERENCE_BODIES = {
        'sun': {'mass': 1.989e30, 'radius': 40},
        'medium_sun': {'mass': 1.0e29, 'radius': 35},
        'small_sun': {'mass': 1.0e28, 'radius': 28},
        'mercury': {'mass': 3.285e23, 'radius': 6},
        'venus': {'mass': 4.867e24, 'radius': 9},
        'earth': {'mass': 5.972e24, 'radius': 10},
        'mars': {'mass': 6.39e23, 'radius': 8},
        'jupiter': {'mass': 1.898e27, 'radius': 25},
        'saturn': {'mass': 5.683e26, 'radius': 22},
        'neptune': {'mass': 8.681e25, 'radius': 16},
        'uranus': {'mass': 1.024e26, 'radius': 15},
    }

    def __init__(self, simulator_client, renderer, camera):
        self.simulator_client = simulator_client
        self.renderer = renderer
        self.camera = camera
        self.planet_counter = 0
        
    def handle_click(self, screen_pos, planet_mass=DEFAULT_MASS):
        """Converte clique em posição física e adiciona planeta com massa especificada"""
        
        self.planet_counter += 1

        # Converter posição da tela para posição física
        world_x = (screen_pos[0] - self.renderer.width/2) * self.renderer.scale / self.camera.zoom + self.camera.x
        world_y = (screen_pos[1] - self.renderer.height/2) * self.renderer.scale / self.camera.zoom + self.camera.y

        # Dados do novo planeta
        planet_name = f"Planeta {self.planet_counter}"
        planet_position = [world_x, world_y]
        planet_velocity = [0, 0]  # Velocidade inicial zero
        planet_radius = self._calculate_radius_from_mass(planet_mass)
        planet_color = random.randint(0, 255), random.randint(0, 255), random.randint(0, 255)  # Cor aleatória
        
        # Enviar comando para adicionar planeta ao backend
        self.simulator_client.add_body(
            name=planet_name,
            mass=planet_mass,
            position=planet_position,
            velocity=planet_velocity,
            radius=planet_radius,
            color=planet_color
        )
        
    def _calculate_radius_from_mass(self, mass):
        """Calcula raio baseado na massa usando uma escala logarítmica suave"""
        # Definir limites de massa e raio
        min_mass = 3.285e23  # Mercúrio
        max_mass = 1.989e30  # Sol
        min_radius = 6       # Raio mínimo (Mercúrio)
        max_radius = 40      # Raio máximo (Sol)
        
        # Usar escala logarítmica para uma distribuição mais natural
        mass_log = math.log10(mass)
        min_mass_log = math.log10(min_mass)
        max_mass_log = math.log10(max_mass)
        
        # Normalizar a massa na escala logarítmica (0 a 1)
        normalized_mass = (mass_log - min_mass_log) / (max_mass_log - min_mass_log)
        
        # Calcular raio baseado na massa normalizada
        radius = min_radius + (max_radius - min_radius) * normalized_mass
        
        # Garantir que o raio fique dentro dos limites
        return max(min_radius, min(max_radius, round(radius)))
            
    def _calculate_comparative_radius(self, mass):
        """Método mantido para compatibilidade - usa o novo método"""
        return self._calculate_radius_from_mass(mass)