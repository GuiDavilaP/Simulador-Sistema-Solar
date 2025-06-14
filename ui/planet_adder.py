import math
import random
from physics.bodies import CelestialBody

DEFAULT_MASS = 1e25  # Massa padrão para novos planetas

class PlanetAdder:
    REFERENCE_BODIES = {
        'sun': {'mass': 1.989e30, 'radius': 40},
        'mercury': {'mass': 3.285e23, 'radius': 6},
        'venus': {'mass': 4.867e24, 'radius': 9},
        'earth': {'mass': 5.972e24, 'radius': 10},
        'mars': {'mass': 6.39e23, 'radius': 8}
    }

    def __init__(self, simulator, renderer, camera):
        self.simulator = simulator
        self.renderer = renderer
        self.camera = camera
        self.planet_counter = 0
        
    def handle_click(self, screen_pos, planet_mass=DEFAULT_MASS):
        """Converte clique em posição física e adiciona planeta"""

        self.planet_counter += 1

        # Converter posição da tela para posição física
        world_x = (screen_pos[0] - self.renderer.width/2) * (self.renderer.scale/self.camera.zoom) + self.camera.x
        world_y = (screen_pos[1] - self.renderer.height/2) * (self.renderer.scale/self.camera.zoom) + self.camera.y

        # Criar novo planeta (com velocidade inicial zero)
        new_planet = CelestialBody(
            name="Planeta " + str(self.planet_counter),
            mass = planet_mass,
            position=[world_x, world_y],
            velocity=[0, 0],  # Initial velocity
            radius=self._calculate_comparative_radius(planet_mass),
            color=(random.randint(50,255), random.randint(50,255), random.randint(50,255))
        )
        
        self.simulator.add_body(new_planet)
        
    def _calculate_comparative_radius(self, mass):
        # Find the closest reference body by mass
        closest_body = min(
            self.REFERENCE_BODIES.values(),
            key=lambda x: abs(math.log10(x['mass']) - math.log10(mass))
        )
        
        # Calculate radius based on mass ratio with closest reference body
        mass_ratio = mass / closest_body['mass']
        # Use cube root to make radius scaling more reasonable
        radius = closest_body['radius'] * (mass_ratio ** (1/3))
        
        # Ensure radius stays within reasonable bounds
        return max(5, min(40, round(radius)))