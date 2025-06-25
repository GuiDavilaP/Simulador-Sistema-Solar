import math

import pygame
import pygame.font

from frontend.renderer import Renderer
from frontend.creator import CelestialBodyCreator
from frontend.camera import Camera
from frontend.backend_proxy import BackendProxy


class MainWindow:
    def __init__(self, backend_path, width=1280, height=720):
        pygame.init()
        pygame.font.init()
        self.screen = pygame.display.set_mode((width, height))
        pygame.display.set_caption("Simulador do Sistema Solar")
        
        self.clock = pygame.time.Clock()
        self.running = False
        self.paused = False
        
        # Inicializar cliente do simulador, renderer e câmera
        self.backend_proxy = BackendProxy(backend_path)
        self.renderer = Renderer(width, height)
        self.camera = Camera(width, height)
        self.creator = CelestialBodyCreator(
            self.backend_proxy, self.renderer, self.camera
        )
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
        self.time_multipliers = [0.1, 0.5, 1.0, 2.0, 5.0, 10.0, 20.0]
        self.time_multiplier_index = 2  # Começa em 1.0x
        
        # Controle do slider de massa
        self.slider_dragging = False
        self.slider_rect = pygame.Rect(width - 320, height - 80, 200, 20)
        self.slider_handle_rect = pygame.Rect(0, 0, 12, 24)
        
        # Configuração da massa (escala logarítmica)
        self.min_mass_log = math.log10(3.285e23)  # Massa de Mercúrio
        self.max_mass_log = math.log10(1.989e30)  # Massa do Sol
        self.current_mass_log = math.log10(5.972e24)  # Começa com massa da Terra
        
        self._update_slider_handle_position()

    def run(self):
        """Loop principal da aplicação"""
        if not self.backend_proxy.start():
            print("Erro: Não foi possível conectar ao simulador backend")
            return
        
        print("Conectado ao backend do simulador!", file=open('debug.log', 'a'))
        
        self.running = True
        try:
            while self.running:
                dt = self.clock.tick(60) / 1000.0  # Tempo delta em segundos
                self._handle_events()
                
                # Verificar se o backend ainda está rodando
                if not self.backend_proxy.is_alive():
                    print("Backend desconectado!")
                    break
                
                # Atualizar câmera
                self.camera.update(dt, self.keys_pressed)
                
                # Atualizar simulação
                self.backend_proxy.update(dt, self.paused)
                
                # Renderizar
                self.screen.fill((0, 0, 0))  # Fundo preto
                
                # Obter corpos do simulador e renderizar
                bodies = self.backend_proxy.get_bodies()
                self.renderer.draw_bodies_with_camera(
                    self.screen, bodies, self.camera, 
                    self.time_multipliers[self.time_multiplier_index]
                )
                # Desenhar UI
                self._draw_ui()
                self._draw_mass_slider()
                
                pygame.display.flip()

        except Exception as e:
            print(f"Erro durante a execução: {e}")
            
        finally:
            self.backend_proxy.stop()
            pygame.quit()
        
    def _update_slider_handle_position(self):
        """Atualiza a posição do handle do slider baseado na massa atual"""
        ratio = (self.current_mass_log - self.min_mass_log) / (self.max_mass_log - self.min_mass_log)
        handle_x = self.slider_rect.x + ratio * (self.slider_rect.width - self.slider_handle_rect.width)
        self.slider_handle_rect.centerx = handle_x + self.slider_handle_rect.width // 2
        self.slider_handle_rect.centery = self.slider_rect.centery
        
    def _get_current_mass(self):
        """Retorna a massa atual baseada na posição do slider"""
        return 10 ** self.current_mass_log
        
    def _get_mass_reference_name(self, mass):
        """Retorna o nome do corpo celeste mais próximo em massa"""
        references = {
            'Mercúrio': 3.285e23,
            'Vênus': 4.867e24,
            'Terra': 5.972e24,
            'Marte': 6.39e23,
            'Júpiter': 1.898e27,
            'Saturno': 5.683e26,
            'Netuno': 8.681e25,
            'Urano': 1.024e26,
            'Sol Pequeno': 1.0e28,
            'Sol Menor': 1.0e29,
            'Sol': 1.989e30
        }
        
        closest_name = min(references.keys(), 
                           key=lambda x: abs(math.log10(references[x]) - math.log10(mass)))
        return closest_name
        
    def _handle_slider_interaction(self, mouse_pos, mouse_pressed):
        """Manipula a interação com o slider de massa"""
        if mouse_pressed[0]:  # Botão esquerdo do mouse pressionado
            if self.slider_rect.collidepoint(mouse_pos) or self.slider_dragging:
                self.slider_dragging = True
                # Calcular nova posição baseada no mouse
                relative_x = mouse_pos[0] - self.slider_rect.x
                relative_x = max(0, min(self.slider_rect.width, relative_x))
                
                # Calcular nova massa
                ratio = relative_x / self.slider_rect.width
                self.current_mass_log = self.min_mass_log + ratio * (self.max_mass_log - self.min_mass_log)
                self._update_slider_handle_position()
        else:
            self.slider_dragging = False
            
    def _draw_mass_slider(self):
        """Desenha o slider de massa na tela"""
        # Fundo do slider
        pygame.draw.rect(self.screen, (60, 60, 60), self.slider_rect)
        pygame.draw.rect(self.screen, (100, 100, 100), self.slider_rect, 2)
        
        # Handle do slider
        pygame.draw.rect(self.screen, (200, 200, 200), self.slider_handle_rect)
        pygame.draw.rect(self.screen, (255, 255, 255), self.slider_handle_rect, 2)
        
        # Texto do slider
        current_mass = self._get_current_mass()
        reference_name = self._get_mass_reference_name(current_mass)
        
        # Título
        title_text = self.small_font.render("Massa do Novo Planeta:", True, (255, 255, 255))
        self.screen.blit(title_text, (self.slider_rect.x, self.slider_rect.y - 40))
        
        # Valor atual
        mass_text = f"{reference_name} ({current_mass:.2e} kg)"
        value_text = self.small_font.render(mass_text, True, (255, 255, 255))
        self.screen.blit(value_text, (self.slider_rect.x, self.slider_rect.y - 20))
        
        # Marcadores de referência
        references = [
            ("Mercúrio", 3.285e23),
            ("Terra", 5.972e24),
            ("Júpiter", 1.898e27),
            ("Sol", 1.989e30)
        ]
        
        for _, mass in references:
            ratio = (math.log10(mass) - self.min_mass_log) / (self.max_mass_log - self.min_mass_log)
            x = self.slider_rect.x + ratio * self.slider_rect.width
            pygame.draw.line(self.screen, (150, 150, 150), (x, self.slider_rect.bottom), (x, self.slider_rect.bottom + 5), 2)
        
    def _handle_events(self):
        """Processa todos os eventos do pygame"""
        mouse_pos = pygame.mouse.get_pos()
        mouse_pressed = pygame.mouse.get_pressed()
        
        # Atualizar slider
        self._handle_slider_interaction(mouse_pos, mouse_pressed)
        
        for event in pygame.event.get():
            print(f"Evento: {event}", file=open('debug.log', 'a'))
            
            if event.type == pygame.QUIT:
                self.running = False
                
            elif event.type == pygame.KEYDOWN:
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
                elif event.button == 1:  # Clique esquerdo
                    # Verificar se não está clicando no slider
                    if not (self.slider_rect.collidepoint(event.pos) or 
                           self.slider_handle_rect.collidepoint(event.pos)):
                        # Adicionar planeta com a massa atual do slider
                        current_mass = self._get_current_mass()
                        self.creator.handle_click(event.pos, current_mass)
    
    def _change_time_speed(self, direction):
        """Altera a velocidade da simulação"""
        self.time_multiplier_index = max(0, min(
            len(self.time_multipliers) - 1, 
            self.time_multiplier_index + direction
        ))
        multiplier = self.time_multipliers[self.time_multiplier_index]
        self.backend_proxy.set_time_scale(multiplier)
    
    def _reset_simulation(self):
        """Reinicia a simulação"""
        self.backend_proxy.reset()
        self.camera.reset()
        self.time_multiplier_index = 2  # Volta para 1.0x
        self.renderer.clear_trails()  # Limpar rastros ao reiniciar

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
        stats = self.backend_proxy.get_stats()
        y_offset = 15
        
        # Status da simulação
        status = "PAUSADO" if self.paused else "RODANDO"
        status_color = (255, 255, 0) if self.paused else (0, 255, 0)
        status_text = self.font.render(f"Status: {status}", True, status_color)
        self.screen.blit(status_text, (15, y_offset))
        y_offset += 25
        
        # Status da conexão
        connection_status = "CONECTADO" if self.backend_proxy.is_alive() else "DESCONECTADO"
        connection_color = (0, 255, 0) if self.backend_proxy.is_alive() else (255, 0, 0)
        connection_text = self.small_font.render(f"Backend: {connection_status}", True, connection_color)
        self.screen.blit(connection_text, (15, y_offset))
        y_offset += 20
        
        # Velocidade da simulação
        speed_multiplier = self.time_multipliers[self.time_multiplier_index]
        days_per_second = f" ({int(10 * speed_multiplier)} dias/segundo)"
        speed_text = self.small_font.render(f"Velocidade: {speed_multiplier}x" + days_per_second, True, (255, 255, 255))
        self.screen.blit(speed_text, (15, y_offset))
        y_offset += 20
        
        # Número de corpos
        bodies_text = self.small_font.render(f"Corpos: {stats.get('total_bodies', 0)}", True, (255, 255, 255))
        self.screen.blit(bodies_text, (15, y_offset))
        y_offset += 20
        
        # Colisões detectadas
        collisions_text = self.small_font.render(f"Colisões: {stats.get('collisions', 0)}", True, (255, 150, 150))
        self.screen.blit(collisions_text, (15, y_offset))
        y_offset += 20
        
        # Controles
        controls_y = self.screen.get_height() - 200
        controls = [
            "ESPAÇO: Pausar/Continuar",
            "R: Reiniciar simulação",
            "M: Mostrar/Ocultar stats",
            "+/-: Acelerar/Desacelerar",
            "Setas: Mover câmera",
            "C: Centralizar no sistema",
            "Clique-esquerdo: Adicionar planeta"
        ]
        control_overlay = pygame.Surface((280, 220))
        control_overlay.set_alpha(128)
        control_overlay.fill((0, 0, 0))
        self.screen.blit(control_overlay, (10, controls_y))
        
        for i, control in enumerate(controls):
            control_text = self.small_font.render(control, True, (200, 200, 200))
            self.screen.blit(control_text, (15, controls_y + 5 + i * 16))
