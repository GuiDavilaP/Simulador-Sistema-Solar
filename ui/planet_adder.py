import math
import random

DEFAULT_MASS = 1e25  # Massa padrão para novos planetas

class PlanetAdder:
    REFERENCE_BODIES = {
        'sun': {'mass': 1.989e30, 'radius': 40},
        'mercury': {'mass': 3.285e23, 'radius': 6},
        'venus': {'mass': 4.867e24, 'radius': 9},
        'earth': {'mass': 5.972e24, 'radius': 10},
        'mars': {'mass': 6.39e23, 'radius': 8}
    }

    def __init__(self, simulator_client, renderer, camera):
        self.simulator_client = simulator_client
        self.renderer = renderer
        self.camera = camera
        self.planet_counter = 0
        
    def handle_click(self, screen_pos, planet_mass=DEFAULT_MASS):
        """Converte clique em posição física e adiciona planeta"""
        
        self.planet_counter += 1

        # Converter posição da tela para posição física
        world_x = (screen_pos[0] - self.renderer.width/2) * self.renderer.scale / self.camera.zoom + self.camera.x
        world_y = (screen_pos[1] - self.renderer.height/2) * self.renderer.scale / self.camera.zoom + self.camera.y

        # Dados do novo planeta
        planet_name = f"Planeta {self.planet_counter}"
        planet_position = [world_x, world_y]
        planet_velocity = [0, 0]  # Velocidade inicial zero
        planet_radius = self._calculate_comparative_radius(planet_mass)
        planet_color = (random.randint(50, 255), random.randint(50, 255), random.randint(50, 255))
        
        # Enviar comando para adicionar planeta ao backend
        self.simulator_client.add_body(
            name=planet_name,
            mass=planet_mass,
            position=planet_position,
            velocity=planet_velocity,
            radius=planet_radius,
            color=planet_color
        )
        
    def _calculate_comparative_radius(self, mass):
        """Calcula raio baseado na massa usando corpos de referência"""
        # Encontrar o corpo de referência mais próximo por massa
        closest_body = min(
            self.REFERENCE_BODIES.values(),
            key=lambda x: abs(math.log10(x['mass']) - math.log10(mass))
        )
        
        # Calcular raio baseado na razão de massa com o corpo de referência
        mass_ratio = mass / closest_body['mass']
        # Usar raiz cúbica para tornar o escalonamento de raio mais razoável
        radius = closest_body['radius'] * (mass_ratio ** (1/3))
        
        # Garantir que o raio fique dentro de limites razoáveis
        return max(5, min(40, round(radius)))