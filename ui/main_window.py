import pygame
import pygame.font
from .renderer import Renderer
from .planet_adder import PlanetAdder
from .camera import Camera
from physics.simulator import SolarSystemSimulator
from physics.bodies import CelestialBody

class MainWindow:
    def __init__(self, width=1280, height=720):
        pygame.init()
        pygame.font.init()
        self.screen = pygame.display.set_mode((width, height))
        pygame.display.set_caption("Simulador do Sistema Solar - Melhorado")
        
        self.clock = pygame.time.Clock()
        self.running = False
        self.paused = False
        
        # Inicializar simulador, renderer e câmera
        self.simulator = SolarSystemSimulator()
        self.renderer = Renderer(width, height)
        self.camera = Camera(width, height)
        self.planet_adder = PlanetAdder(self.simulator, self.renderer, self.camera)
        
        # Controle de interface
        self.show_stats = True
        self.font = pygame.font.Font(None, 24)
        self.small_font = pygame.font.Font(None, 18)
        
        # Estado das teclas
        self.keys_pressed = {
            'left': False,
            'right': False,
            'up': False,
            'down': False,
            'shift': False
        }
        
        # Controle de velocidade da simulação
        self.time_multipliers = [0.1, 0.5, 1.0, 2.0, 5.0, 10.0, 50.0, 100.0]
        self.current_multiplier_index = 2  # Começa em 1.0x
        
        # Configurar sistema solar inicial
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
            position=[5.79e10, 0],  # 0.387 UA
            velocity=[0, 47.36e3],
            radius=6,
            color=(169, 169, 169)
        )

        # Vênus
        venus = CelestialBody(
            name="Venus",
            mass=4.867e24,
            position=[1.082e11, 0],  # 0.723 UA
            velocity=[0, 35.02e3],
            radius=9,
            color=(255, 198, 73)
        )

        # Terra
        earth = CelestialBody(
            name="Terra",
            mass=5.972e24,
            position=[1.496e11, 0],  # 1.0 UA
            velocity=[0, 29.8e3],
            radius=10,
            color=(100, 149, 237)
        )
        
        # Marte
        mars = CelestialBody(
            name="Marte",
            mass=6.39e23,
            position=[2.279e11, 0],  # 1.52 UA
            velocity=[0, 24.1e3],
            radius=8,
            color=(205, 92, 92)
        )
        
        # Júpiter
        jupiter = CelestialBody(
            name="Jupiter",
            mass=1.898e27,
            position=[7.786e11, 0],  # 5.20 UA
            velocity=[0, 13.1e3],
            radius=25,
            color=(255, 165, 0)
        )

        # Saturno
        saturn = CelestialBody(
            name="Saturno",
            mass=5.683e26,
            position=[1.434e12, 0],  # 9.58 UA
            velocity=[0, 9.7e3],
            radius=22,
            color=(238, 232, 205)
        )

        # Urano
        uranus = CelestialBody(
            name="Urano",
            mass=8.681e25,
            position=[2.871e12, 0],  # 19.18 UA
            velocity=[0, 6.8e3],
            radius=16,
            color=(173, 216, 230)
        )

        # Netuno
        neptune = CelestialBody(
            name="Netuno",
            mass=1.024e26,
            position=[4.495e12, 0],  # 30.07 UA
            velocity=[0, 5.4e3],
            radius=15,
            color=(0, 0, 128)
        )

        # Adicionar todos os corpos ao simulador
        self.simulator.add_body(sun)
        self.simulator.add_body(mercury)
        self.simulator.add_body(venus)
        self.simulator.add_body(earth)
        self.simulator.add_body(mars)
        self.simulator.add_body(jupiter)
        self.simulator.add_body(saturn)
        self.simulator.add_body(uranus)
        self.simulator.add_body(neptune)
        
    def _handle_events(self):
        """Processa todos os eventos do pygame"""
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                self.running = False
                
            elif event.type == pygame.KEYDOWN:
                # Controles de simulação
                if event.key == pygame.K_SPACE:
                    self.paused = not self.paused
                elif event.key == pygame.K_r:
                    self._reset_simulation()
                elif event.key == pygame.K_s:
                    self.show_stats = not self.show_stats
                elif event.key == pygame.K_PLUS or event.key == pygame.K_EQUALS:
                    self._change_time_speed(1)
                elif event.key == pygame.K_MINUS:
                    self._change_time_speed(-1)
                
                # Controles de câmera
                elif event.key == pygame.K_LEFT:
                    self.keys_pressed['left'] = True
                elif event.key == pygame.K_RIGHT:
                    self.keys_pressed['right'] = True
                elif event.key == pygame.K_UP:
                    self.keys_pressed['up'] = True
                elif event.key == pygame.K_DOWN:
                    self.keys_pressed['down'] = True
                
                # Teclas especiais de câmera
                elif event.key == pygame.K_c:
                    self.camera.center_on_system(self.simulator.bodies)
                    
            elif event.type == pygame.KEYUP:
                # Parar movimento da câmera
                if event.key == pygame.K_LEFT:
                    self.keys_pressed['left'] = False
                elif event.key == pygame.K_RIGHT:
                    self.keys_pressed['right'] = False
                elif event.key == pygame.K_UP:
                    self.keys_pressed['up'] = False
                elif event.key == pygame.K_DOWN:
                    self.keys_pressed['down'] = False
                    
            elif event.type == pygame.MOUSEBUTTONDOWN:
                if event.button in (4, 5):  # Mouse wheel
                    zoom_factor = 1.1 if event.button == 4 else 0.9
                    new_zoom = self.camera.zoom * zoom_factor
                    self.camera.set_zoom(new_zoom)
                else:
                    self.planet_adder.handle_click(event.pos)
    
    def _change_time_speed(self, direction):
        """Altera a velocidade da simulação"""
        self.current_multiplier_index = max(0, min(
            len(self.time_multipliers) - 1, 
            self.current_multiplier_index + direction
        ))
        multiplier = self.time_multipliers[self.current_multiplier_index]
        self.simulator.time_scale = 86400 * 10 * multiplier  # 10 dias/segundo base
    
    def _reset_simulation(self):
        """Reinicia a simulação"""
        self.simulator = SolarSystemSimulator()
        self.planet_adder = PlanetAdder(self.simulator, self.renderer, self.camera)
        self._initialize_solar_system()
        self.camera.reset()
        self.camera.set_zoom(1.0)
    
    def _draw_ui(self):
        """Desenha a interface do usuário"""
        if not self.show_stats:
            return
            
        # Fundo semi-transparente para o texto
        overlay = pygame.Surface((300, 200))
        overlay.set_alpha(128)
        overlay.fill((0, 0, 0))
        self.screen.blit(overlay, (10, 10))
        
        # Estatísticas do sistema
        stats = self.simulator.get_system_stats()
        y_offset = 15
        
        # Status da simulação
        status = "PAUSADO" if self.paused else "RODANDO"
        status_color = (255, 255, 0) if self.paused else (0, 255, 0)
        status_text = self.font.render(f"Status: {status}", True, status_color)
        self.screen.blit(status_text, (15, y_offset))
        y_offset += 25
        
        # Velocidade da simulação
        speed_multiplier = self.time_multipliers[self.current_multiplier_index]
        speed_text = self.small_font.render(f"Velocidade: {speed_multiplier}x", True, (255, 255, 255))
        self.screen.blit(speed_text, (15, y_offset))
        y_offset += 20
        
        # Número de corpos
        bodies_text = self.small_font.render(f"Corpos: {stats['total_bodies']}", True, (255, 255, 255))
        self.screen.blit(bodies_text, (15, y_offset))
        y_offset += 20
        
        # Corpos removidos
        removed_text = self.small_font.render(f"Removidos: {stats['bodies_removed']}", True, (255, 100, 100))
        self.screen.blit(removed_text, (15, y_offset))
        y_offset += 20
        
        # Colisões detectadas
        collisions_text = self.small_font.render(f"Colisões: {stats['collisions_detected']}", True, (255, 150, 150))
        self.screen.blit(collisions_text, (15, y_offset))
        y_offset += 20
        
        # Ejeções detectadas
        ejections_text = self.small_font.render(f"Ejeções: {stats['ejections_detected']}", True, (150, 150, 255))
        self.screen.blit(ejections_text, (15, y_offset))
        y_offset += 20
        
        # Controles
        controls_y = self.screen.get_height() - 160
        controls = [
            "ESPAÇO: Pausar/Continuar",
            "R: Reiniciar simulação",
            "S: Mostrar/Ocultar stats",
            "+/-: Acelerar/Desacelerar",
            "Setas: Mover câmera",
            "C: Centralizar no sistema",
            "Clique-esquerdo: Adicionar planeta"
        ]
        
        control_overlay = pygame.Surface((280, 180))
        control_overlay.set_alpha(128)
        control_overlay.fill((0, 0, 0))
        self.screen.blit(control_overlay, (10, controls_y))
        
        for i, control in enumerate(controls):
            control_text = self.small_font.render(control, True, (200, 200, 200))
            self.screen.blit(control_text, (15, controls_y + 5 + i * 16))
    
    def run(self):
        """Loop principal da aplicação"""
        self.running = True
        while self.running:
            dt = self.clock.tick(60) / 1000.0  # Delta time em segundos
            
            self._handle_events()
            
            # Atualizar câmera
            self.camera.update(dt, self.keys_pressed)
            
            # Atualizar simulação (apenas se não estiver pausado)
            if not self.paused:
                self.simulator.update(dt)
            
            # Renderizar
            self.screen.fill((0, 0, 0))  # Fundo preto
            self.renderer.draw_with_camera(self.screen, self.simulator.bodies, self.camera)
            self._draw_ui()
            
            pygame.display.flip()
        
        pygame.quit()