import pygame
import pygame.gfxdraw

class Renderer:
    def __init__(self, width, height):
        self.width = width
        self.height = height
        self.scale = 1e9  # Escala para converter metros para pixels
    
    def draw_bodies_with_camera(self, screen, bodies_data, camera):
        """Renderiza corpos celestes usando dados JSON do backend"""
        for body_data in bodies_data:
            # Calcular posição na tela considerando a câmera
            screen_x = (body_data['position'][0] - camera.x) / (self.scale / camera.zoom) + self.width / 2
            screen_y = (body_data['position'][1] - camera.y) / (self.scale / camera.zoom) + self.height / 2
            
            # Calcular raio na tela
            radius_on_screen = max(1.0, body_data['radius'] * camera.zoom)
            
            # Desenhar apenas se estiver visível na tela
            margin = radius_on_screen + 5  # Margem maior para suavização
            if (-margin <= screen_x <= self.width + margin and
                -margin <= screen_y <= self.height + margin):
                
                # Converter cor de lista para tupla se necessário
                color = tuple(body_data['color']) if isinstance(body_data['color'], list) else body_data['color']
                
                # Usar coordenadas float para melhor precisão
                center_x = int(round(screen_x))
                center_y = int(round(screen_y))
                radius_int = int(round(radius_on_screen))
                
                # Desenhar corpo principal
                if radius_int >= 1:
                    # Círculo preenchido anti-aliased
                    pygame.gfxdraw.filled_circle(screen, center_x, center_y, radius_int, color)
                    pygame.gfxdraw.aacircle(screen, center_x, center_y, radius_int, color)
                
                # Desenhar borda suave para melhor visibilidade
                if radius_int > 3:  # Só desenhar borda se o planeta for grande
                    border_color = (200, 200, 200)
                    
                    # Borda externa anti-aliased
                    pygame.gfxdraw.aacircle(screen, center_x, center_y, radius_int, border_color)
                    
                    # Borda interna opcional para planetas maiores
                    if radius_int > 8:
                        inner_border = max(1, radius_int - 1)
                        pygame.gfxdraw.aacircle(screen, center_x, center_y, inner_border, border_color)

                # Desenhar nome do corpo se zoom suficiente
                if camera.zoom > 0.5 and radius_int > 8:
                    self._draw_body_label(screen, body_data["name"], screen_x, screen_y + radius_int + 8)
        
    def _draw_body_label(self, screen, name, x, y):
        """Desenha o nome de um corpo celestial com anti-aliasing"""
        # Usar fonte com anti-aliasing
        font = pygame.font.Font(None, 20)
        text = font.render(name, True, (200, 200, 200))  # True = anti-aliasing
        text_rect = text.get_rect(center=(int(round(x)), int(round(y))))
        
        # Desenhar sombra sutil para melhor legibilidade
        shadow_color = (50, 50, 50)
        shadow_text = font.render(name, True, shadow_color)
        shadow_rect = text_rect.copy()
        shadow_rect.x += 1
        shadow_rect.y += 1
        screen.blit(shadow_text, shadow_rect)
        
        # Desenhar texto principal
        screen.blit(text, text_rect)

    def _draw_scale_indicator(self, screen, camera):
        """Desenha um indicador de escala no canto da tela com anti-aliasing"""
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
        
        # Desenhar barra com linhas mais suaves
        line_color = (200, 200, 200)  # Cor mais suave
        
        # Linha principal
        pygame.draw.line(screen, line_color, 
                        (bar_x, bar_y), (bar_x + scale_length_pixels, bar_y), 2)
        
        # Marcadores nas extremidades
        pygame.draw.line(screen, line_color, 
                        (bar_x, bar_y - 5), (bar_x, bar_y + 5), 2)
        pygame.draw.line(screen, line_color, 
                        (bar_x + scale_length_pixels, bar_y - 5), 
                        (bar_x + scale_length_pixels, bar_y + 5), 2)
        
        # Desenhar texto da escala com anti-aliasing
        font = pygame.font.Font(None, 18)
        scale_text = f"{scale_value:.1f} {scale_unit}"
        text_surface = font.render(scale_text, True, line_color)  # True = anti-aliasing
        text_rect = text_surface.get_rect(center=(bar_x + scale_length_pixels // 2, bar_y - 15))
        screen.blit(text_surface, text_rect)

    def draw_smooth_circle(self, screen, color, center, radius):
        """Função auxiliar para desenhar círculos suaves"""
        if radius < 1:
            return
            
        center_x, center_y = int(round(center[0])), int(round(center[1]))
        radius_int = int(round(radius))
        
        # Círculo preenchido
        pygame.gfxdraw.filled_circle(screen, center_x, center_y, radius_int, color)
        # Borda anti-aliased
        pygame.gfxdraw.aacircle(screen, center_x, center_y, radius_int, color)