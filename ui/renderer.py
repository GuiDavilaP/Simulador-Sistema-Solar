import pygame
import math

SCALE = 1e9  # Escala padrão de 1 pixel = 1 milhão de km
FONT_SIZE = 22

class Renderer:
    def __init__(self, width, height):
        self.width = width
        self.height = height
        self.zoom = 1.0
        self.scale = SCALE
        self.font = pygame.font.Font(None, FONT_SIZE)
        
    def set_zoom(self, zoom):
        self.zoom = zoom

    def world_to_screen(self, position):
        # Converter coordenadas do mundo para coordenadas da tela
        screen_x = self.width / 2 + (position[0] / self.scale) * self.zoom
        screen_y = self.height / 2 + (position[1] / self.scale) * self.zoom
        return (int(screen_x), int(screen_y))

    # Escala o raio do planeta de acordo com o zoom
    def get_scaled_radius(self, radius):
        return max(1, int(radius * self.zoom))

    def draw(self, screen, bodies):
        """Desenha todos os corpos na superfície"""
        screen.fill((0, 0, 0))  # Fundo preto
        
        for body in bodies:
            # Converter posição física para posição na tela
            screen_pos = self.world_to_screen(body.position)
            scaled_radius = self.get_scaled_radius(body.radius)
            
            # Desenhar o corpo celeste
            pygame.draw.circle(
                screen,
                body.color,
                screen_pos,
                scaled_radius
            )
            
            # Desenha nome do corpo
            if hasattr(body, 'name'):
                name_text = self.font.render(body.name, True, "White")
                # Posicionar ao lado do planeta, ajustando com o zoom
                text_pos = (
                    screen_pos[0] + scaled_radius + 5,  # 5 pixels à direita do planeta
                    screen_pos[1] - name_text.get_height() // 2  # Centralizado verticalmente
                )
                screen.blit(name_text, text_pos)
        
        # Debug: mostra escala
        font = pygame.font.SysFont(None, 24)
        scale_text = font.render(f"1px = {self.scale/1e9} milhões de km", True, (255,255,255))
        screen.blit(scale_text, (10, 10))