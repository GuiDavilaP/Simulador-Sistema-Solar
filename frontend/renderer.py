import pygame
import pygame.gfxdraw
import random

MIN_TRAIL_LENGTH = 5  # Comprimento mínimo do rastro
MAX_TRAIL_LENGTH = 200  # Comprimento máximo do rastro


class Renderer:
    def __init__(self, width, height):
        self.width = width
        self.height = height
        self.scale = 1e9  # Escala para converter metros para pixels
        
        # Sistema de rastros simplificado
        self.trails = {}  # {body_name: [(x, y), ...]}
        self.trail_update_counter = 0  # Contador para atualizar rastros menos frequentemente
        
        # Estrelas de fundo
        self.background_stars = self._generate_stars(150)
        
    def _generate_stars(self, count):
        """Gera estrelas aleatórias para o fundo"""
        stars = []
        for _ in range(count):
            star = {
                'x': random.randint(0, self.width),
                'y': random.randint(0, self.height),
                'brightness': random.randint(50, 200),
                'size': random.choice([1, 1, 1, 2, 2, 3])  # Maioria pequenas
            }
            stars.append(star)
        return stars
    
    def _update_trails(self, bodies_data, simulation_speed=1):
        """Atualiza os rastros dos planetas de forma otimizada"""

        self.trail_update_counter += 1

        # Buffer de inicialização de rastros
        if len(self.trails) == 0 and self.trail_update_counter < 5:
            return

        # Não atualizar rastros com muita frequência
        if self.trail_update_counter < max(1, 10 / simulation_speed):
            return
        self.trail_update_counter = 0

        # Adicionar novos pontos aos rastros
        active_bodies = set()
        for body_data in bodies_data:
            name = body_data['name']
            active_bodies.add(name)
            
            # Inicializar rastro se não existir
            if name not in self.trails:
                self.trails[name] = []
                continue

            trail_point = (body_data['position'][0], body_data['position'][1])
            
            # Evitar pontos duplicados muito próximos
            if not self.trails[name] or self._distance(trail_point, self.trails[name][-1]) > 1e8:
                self.trails[name].append(trail_point)
                
                # Limitar tamanho do rastro
                if len(self.trails[name]) > max(MIN_TRAIL_LENGTH,(MAX_TRAIL_LENGTH // simulation_speed)):
                    self.trails[name].pop(0)
        
        # Remover rastros de corpos que não existem mais
        inactive_bodies = set(self.trails.keys()) - active_bodies
        for name in inactive_bodies:
            del self.trails[name]
    
    def _distance(self, p1, p2):
        """Calcula distância entre dois pontos"""
        return ((p1[0] - p2[0])**2 + (p1[1] - p2[1])**2)**0.5
    
    def _draw_stars(self, screen, camera):
        """Desenha estrelas de fundo fixas"""
        for star in self.background_stars:
            # Calcular posição das estrelas considerando movimento da câmera
            # As estrelas se movem muito lentamente com a câmera para parecer distante
            parallax_factor = 0.1
            star_x = star['x'] - (camera.x * parallax_factor / self.scale)
            star_y = star['y'] - (camera.y * parallax_factor / self.scale)
            
            # Fazer as estrelas "envolveren" a tela
            star_x = star_x % self.width
            star_y = star_y % self.height
            
            # Desenhar estrela
            color = (star['brightness'], star['brightness'], star['brightness'])
            
            if star['size'] == 1:
                screen.set_at((int(star_x), int(star_y)), color)
            else:
                pygame.gfxdraw.filled_circle(screen, int(star_x), int(star_y), star['size'], color)
                if star['size'] > 1:
                    pygame.gfxdraw.aacircle(screen, int(star_x), int(star_y), star['size'], color)
    
    def _draw_trails(self, screen, camera):
        """Desenha rastros dos planetas de forma otimizada"""

        for name, trail in self.trails.items():
            if len(trail) < 2:
                continue
            
            # Converter pontos do rastro para coordenadas de tela
            screen_points = []
            for i, (world_x, world_y) in enumerate(trail):
                screen_x = (world_x - camera.x) / (self.scale / camera.zoom) + self.width / 2
                screen_y = (world_y - camera.y) / (self.scale / camera.zoom) + self.height / 2
                
                # Verificação simples de visibilidade
                if -100 <= screen_x <= self.width + 100 and -100 <= screen_y <= self.height + 100:
                    screen_points.append((int(screen_x), int(screen_y), i))
            
            if len(screen_points) < 2:
                continue
            
            # Desenhar rastro com linhas simples
            for i in range(len(screen_points) - 1):
                x1, y1, idx1 = screen_points[i]
                x2, y2, idx2 = screen_points[i + 1]
                
                # Fade-out usando progressão exponencial
                progress = i / (len(screen_points) - 1)
                alpha = int(180 * (progress ** 2.5))

                try:
                    # Desenhar linha mais grossa com anti-aliasing
                    pygame.draw.line(screen, (255, 255, 255, alpha), 
                                (x1, y1), (x2, y2), 1)
                    
                    # Adicionar linha mais fina por cima para suavização
                    if alpha > 10:  # Only draw overlay for visible segments
                        pygame.draw.aaline(screen, (255, 255, 255, alpha//3), 
                                        (x1, y1), (x2, y2))
                except (ValueError, OverflowError):
                    continue
    
    def draw_bodies_with_camera(self, screen, bodies_data, camera, simulation_speed=1):
        """Renderiza corpos celestes usando dados JSON do backend"""
        # Desenhar estrelas de fundo primeiro
        self._draw_stars(screen, camera)
        
        # Atualizar e desenhar rastros
        self._update_trails(bodies_data, simulation_speed)
        self._draw_trails(screen, camera)
        
        # Desenhar corpos celestes
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
        
    def clear_trails(self):
        """Limpa todos os rastros (útil ao reiniciar simulação)"""
        self.trails.clear()