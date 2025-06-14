import pygame
import pygame.font
from .renderer import Renderer
from .planet_adder import PlanetAdder
from .camera import Camera
from .simulator_client import SimulatorClient

class MainWindow:
    def __init__(self, width=1280, height=720, backend_path="python simulator_backend.py"):
        pygame.init()
        pygame.font.init()
        self.screen = pygame.display.set_mode((width, height))
        pygame.display.set_caption("Simulador do Sistema Solar")
        
        self.clock = pygame.time.Clock()
        self.running = False
        self.paused = False
        
        # Inicializar cliente do simulador, renderer e câmera
        self.simulator_client = SimulatorClient(backend_path)
        self.renderer = Renderer(width, height)
        self.camera = Camera(width, height)
        self.planet_adder = PlanetAdder(self.simulator_client, self.renderer, self.camera)
        
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

        # Mapeamento de teclas para direções
        self.key_mapping = {
            pygame.K_LEFT: 'left',
            pygame.K_RIGHT: 'right',
            pygame.K_UP: 'up',
            pygame.K_DOWN: 'down',
            pygame.K_a: 'left',
            pygame.K_d: 'right',
            pygame.K_w: 'up',
            pygame.K_s: 'down'
        }
        
        # Controle de velocidade da simulação
        self.time_multipliers = [0.1, 0.5, 1.0, 2.0, 5.0, 10.0]
        self.current_multiplier_index = 2  # Começa em 1.0x
        
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
                elif event.key == pygame.K_m:
                    self.show_stats = not self.show_stats
                elif event.key == pygame.K_PLUS or event.key == pygame.K_EQUALS:
                    self._change_time_speed(1)
                elif event.key == pygame.K_MINUS:
                    self._change_time_speed(-1)
                elif event.key == pygame.K_c:
                    self.camera.reset()
                
                # Atualizar estado das teclas de movimento quando pressionadas
                if event.key in self.key_mapping:
                    self.keys_pressed[self.key_mapping[event.key]] = True
                    
            elif event.type == pygame.KEYUP:
                # Atualizar estado das teclas de movimento quando soltas
                if event.key in self.key_mapping:
                    self.keys_pressed[self.key_mapping[event.key]] = False
                    
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
        self.simulator_client.set_time_scale(multiplier)
    
    def _reset_simulation(self):
        """Reinicia a simulação"""
        self.simulator_client.reset()
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
        stats = self.simulator_client.get_stats()
        y_offset = 15
        
        # Status da simulação
        status = "PAUSADO" if self.paused else "RODANDO"
        status_color = (255, 255, 0) if self.paused else (0, 255, 0)
        status_text = self.font.render(f"Status: {status}", True, status_color)
        self.screen.blit(status_text, (15, y_offset))
        y_offset += 25
        
        # Status da conexão
        connection_status = "CONECTADO" if self.simulator_client.is_alive() else "DESCONECTADO"
        connection_color = (0, 255, 0) if self.simulator_client.is_alive() else (255, 0, 0)
        connection_text = self.small_font.render(f"Backend: {connection_status}", True, connection_color)
        self.screen.blit(connection_text, (15, y_offset))
        y_offset += 20
        
        # Velocidade da simulação
        speed_multiplier = self.time_multipliers[self.current_multiplier_index]
        speed_text = self.small_font.render(f"Velocidade: {speed_multiplier}x", True, (255, 255, 255))
        self.screen.blit(speed_text, (15, y_offset))
        y_offset += 20
        
        # Número de corpos
        bodies_text = self.small_font.render(f"Corpos: {stats.get('total_bodies', 0)}", True, (255, 255, 255))
        self.screen.blit(bodies_text, (15, y_offset))
        y_offset += 20
        
        # Corpos removidos
        removed_text = self.small_font.render(f"Removidos: {stats.get('bodies_removed', 0)}", True, (255, 100, 100))
        self.screen.blit(removed_text, (15, y_offset))
        y_offset += 20
        
        # Colisões detectadas
        collisions_text = self.small_font.render(f"Colisões: {stats.get('collisions_detected', 0)}", True, (255, 150, 150))
        self.screen.blit(collisions_text, (15, y_offset))
        y_offset += 20
        
        # Ejeções detectadas
        ejections_text = self.small_font.render(f"Ejeções: {stats.get('ejections_detected', 0)}", True, (150, 150, 255))
        self.screen.blit(ejections_text, (15, y_offset))
        y_offset += 20
        
        # Controles
        controls_y = self.screen.get_height() - 160
        controls = [
            "ESPAÇO: Pausar/Continuar",
            "R: Reiniciar simulação",
            "M: Mostrar/Ocultar stats",
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
        # Inicializar conexão com o backend
        if not self.simulator_client.start():
            print("Erro: Não foi possível conectar ao simulador backend")
            return
        
        self.running = True
        try:
            while self.running:
                dt = self.clock.tick(60) / 1000.0  # Delta time em segundos
                
                self._handle_events()
                
                # Verificar se o backend ainda está rodando
                if not self.simulator_client.is_alive():
                    print("Backend desconectado!")
                    break
                
                # Atualizar câmera
                self.camera.update(dt, self.keys_pressed)
                
                # Atualizar simulação
                self.simulator_client.update(dt, self.paused)
                
                # Renderizar
                self.screen.fill((0, 0, 0))  # Fundo preto
                
                # Obter corpos do simulador e renderizar
                bodies = self.simulator_client.get_bodies()
                self.renderer.draw_bodies_with_camera(self.screen, bodies, self.camera)
                
                self._draw_ui()
                
                pygame.display.flip()
        
        finally:
            self.simulator_client.stop()
            pygame.quit()