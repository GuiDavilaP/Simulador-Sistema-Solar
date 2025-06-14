import pygame
from .renderer import Renderer
from .planet_adder import PlanetAdder
from physics.simulator import SolarSystemSimulator
from physics.bodies import CelestialBody

class MainWindow:
    def __init__(self, width=1280, height=720):
        pygame.init()
        self.screen = pygame.display.set_mode((width, height))
        pygame.display.set_caption("Simulador do Sistema Solar")
        
        self.clock = pygame.time.Clock()
        self.running = False
        
        # Inicializar simulador e renderer
        self.simulator = SolarSystemSimulator()
        self.renderer = Renderer(width, height)
        self.planet_adder = PlanetAdder(self.simulator, self.renderer)
        
        # Controle de zoom
        self.zoom = 1.0

        # Configurar sistema solar inicial
        self._initialize_solar_system()
        
    def _initialize_solar_system(self):
        # Sol (central, quase estático)
        sun = CelestialBody(
            name="Sol",
            mass=1.989e30,
            position=[0, 0],
            velocity=[0, 0],
            radius=40,
            color=(255, 255, 0)
        )
        
        # Mercúrio
        mercury = CelestialBody(
            name="Mercurio",
            mass=3.285e23,
            position=[5.79e10, 0],  # ~0.387 UA
            velocity=[0, 47.36e3],  # ~47.36 km/s
            radius=6,
            color=(169, 169, 169)  # Cinza
        )

        # Vênus
        venus = CelestialBody(
            name="Venus",
            mass=4.867e24,
            position=[1.082e11, 0],  # ~0.723 UA
            velocity=[0, 35.02e3],  # ~35.02 km/s
            radius=9,
            color=(255, 198, 73)  # Amarelo alaranjado
        )

        # Terra
        earth = CelestialBody(
            name="Terra",
            mass=5.972e24,
            position=[1.496e11, 0],  # ~1 UA
            velocity=[0, 29.8e3],    # ~29.8 km/s
            radius=10,
            color=(0, 100, 255)
        )
        
        # Marte
        mars = CelestialBody(
            name="Marte",
            mass=6.39e23,
            position=[2.279e11, 0],  # ~1.52 UA
            velocity=[0, 24.1e3],   # ~24.1 km/s
            radius=8,
            color=(255, 100, 0)
        )

        self.simulator.add_body(sun)
        self.simulator.add_body(mercury)
        self.simulator.add_body(venus)
        self.simulator.add_body(earth)
        self.simulator.add_body(mars)
        
    def run(self):
        self.running = True
        while self.running:
            dt = self.clock.tick(60) / 1000.0  # Delta time em segundos
            # Processar eventos
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.running = False
                elif event.type == pygame.MOUSEBUTTONDOWN:
                    if event.button in (4, 5):  # Mouse wheel up (4) or down (5)
                        zoom_factor = 1.1 if event.button == 4 else 0.9
                        self.zoom *= zoom_factor
                        self.renderer.set_zoom(self.zoom)
                    else:
                        self.planet_adder.handle_click(event.pos)
            
            # Atualizar simulação
            self.simulator.update(dt)
            
            # Renderizar
            self.renderer.draw(self.screen, self.simulator.bodies)
            pygame.display.flip()
        
        pygame.quit()