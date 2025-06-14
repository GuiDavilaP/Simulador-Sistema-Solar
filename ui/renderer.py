import pygame
import math

class Renderer:
    def __init__(self, width, height):
        self.width = width
        self.height = height
        
        # Escala para converter de metros para pixels
        # 1 pixel = ~1e9 metros (1 milhão de km por pixel aproximadamente)
        self.scale = 1e9
        
        # Cores e estilos
        self.background_color = (0, 0, 0)
        self.trail_color = (255,0,0,50)
        
        # Sistema de trilhas
        self.show_trails = False
        self.trails = {}  # Dicionário para armazenar trilhas de cada corpo
        self.max_trail_length = 1000
    
    def draw_with_camera(self, screen, bodies, camera):
        """Desenha todos os corpos usando o sistema de câmera"""
        screen.fill(self.background_color)
        
        # Desenhar trilhas se habilitadas
        if self.show_trails:
            self._draw_trails(screen, camera)
        
        # Desenhar corpos
        for body in bodies:
            self._draw_body(screen, body, camera)
        
        # Desenhar elementos de UI relacionados ao espaço
        self._draw_scale_indicator(screen, camera)
    
    def _draw_body(self, screen, body, camera):
        """Desenha um corpo celestial na tela"""
        # Converter posição do mundo para coordenadas da tela
        screen_x, screen_y = camera.world_to_screen(
            body.position[0], body.position[1], self.scale
        )
        
        # Verificar se o corpo está visível na tela
        margin = body.radius * 2
        if (screen_x < -margin or screen_x > self.width + margin or
            screen_y < -margin or screen_y > self.height + margin):
            return  # Corpo fora da tela, não desenhar
        
        # Calcular raio na tela baseado no zoom
        screen_radius = max(2, int(body.radius * camera.zoom))
        
        # Desenhar o corpo
        pygame.draw.circle(
            screen, 
            body.color, 
            (int(screen_x), int(screen_y)), 
            screen_radius
        )
        
        # Desenhar borda para melhor visibilidade
        if screen_radius > 5:
            pygame.draw.circle(
                screen, 
                (255, 255, 255), 
                (int(screen_x), int(screen_y)), 
                screen_radius, 
                1
            )
        
        # Desenhar nome do corpo se zoom suficiente
        if camera.zoom > 0.5 and screen_radius > 8:
            self._draw_body_label(screen, body.name, screen_x, screen_y + screen_radius + 5)
        
        # Atualizar trilha
        if self.show_trails:
            self._update_trail(body)
    
    def _draw_body_label(self, screen, name, x, y):
        """Desenha o nome de um corpo celestial"""
        font = pygame.font.Font(None, 20)
        text = font.render(name, True, (200, 200, 200))
        text_rect = text.get_rect(center=(int(x), int(y)))
        screen.blit(text, text_rect)
    
    def _draw_scale_indicator(self, screen, camera):
        """Desenha um indicador de escala no canto da tela"""
        # Calcular tamanho da barra de escala
        scale_length_pixels = 100  # pixels
        scale_length_world = scale_length_pixels * self.scale / camera.zoom
        
        # Converter para unidades mais legíveis
        if scale_length_world > 1.496e11:  # Maior que 1 UA
            scale_value = scale_length_world / 1.496e11
            scale_unit = "UA"
        elif scale_length_world > 1e9:  # Maior que 1 milhão de km
            scale_value = scale_length_world / 1e9
            scale_unit = "Mkm"
        elif scale_length_world > 1e6:  # Maior que 1000 km
            scale_value = scale_length_world / 1e6
            scale_unit = "km"
        else:
            scale_value = scale_length_world
            scale_unit = "m"
        
        # Posição da barra de escala
        bar_x = self.width - 120
        bar_y = self.height - 30
        
        # Desenhar barra
        pygame.draw.line(screen, (255, 255, 255), 
                        (bar_x, bar_y), (bar_x + scale_length_pixels, bar_y), 2)
        pygame.draw.line(screen, (255, 255, 255), 
                        (bar_x, bar_y - 5), (bar_x, bar_y + 5), 2)
        pygame.draw.line(screen, (255, 255, 255), 
                        (bar_x + scale_length_pixels, bar_y - 5), 
                        (bar_x + scale_length_pixels, bar_y + 5), 2)
        
        # Desenhar texto da escala
        font = pygame.font.Font(None, 18)
        scale_text = f"{scale_value:.1f} {scale_unit}"
        text_surface = font.render(scale_text, True, (255, 255, 255))
        text_rect = text_surface.get_rect(center=(bar_x + scale_length_pixels // 2, bar_y - 15))
        screen.blit(text_surface, text_rect)