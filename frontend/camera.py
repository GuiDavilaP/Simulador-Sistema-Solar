class Camera:
    def __init__(self, width, height, max_distance=3e12):
        self.width = width
        self.height = height
        
        # Posição da câmera no mundo
        self.x = 0.0
        self.y = 0.0
        
        # Limites de movimento (em metros do mundo)
        self.max_distance = max_distance  # ~6.7 UA, um pouco além de Júpiter
        
        self.fast_speed_multiplier = 5.0
        
        # Zoom
        self.zoom = 1.0

        # Velocidade de movimento da câmera (metros por segundo no mundo)
        self.base_speed = 5e10  * (1 / self.zoom * 10)
        
    def update(self, dt, keys_pressed):
        """Atualiza a posição da câmera baseada nas teclas pressionadas"""
        # A velocidade é inversamente proporcional ao zoom para manter movimento consistente
        current_speed = self.base_speed / self.zoom
        
        # Movimento horizontal
        if keys_pressed.get('left', False):
            self.x -= current_speed * dt
        if keys_pressed.get('right', False):
            self.x += current_speed * dt
            
        # Movimento vertical
        if keys_pressed.get('up', False):
            self.y -= current_speed * dt
        if keys_pressed.get('down', False):
            self.y += current_speed * dt
            
        # Aplicar limites
        self._apply_limits()
    
    def _apply_limits(self):
        """Aplica os limites de movimento da câmera"""
        # Converter limite de pixels para metros do mundo
        world_limit = self.max_distance
        
        # Limitar posição
        self.x = max(-world_limit, min(world_limit, self.x))
        self.y = max(-world_limit, min(world_limit, self.y))
    
    def set_zoom(self, zoom):
        """Define o zoom da câmera"""
        self.zoom = max(0.25, min(5.0, zoom))
    
    def world_to_screen(self, world_x, world_y, scale):
        """Converte coordenadas do mundo para coordenadas da tela"""
        # Aplicar offset da câmera
        relative_x = world_x - self.x
        relative_y = world_y - self.y
        
        # Converter para coordenadas da tela
        screen_x = (relative_x * self.zoom / scale) + self.width / 2
        screen_y = (relative_y * self.zoom / scale) + self.height / 2
        
        return screen_x, screen_y
    
    def screen_to_world(self, screen_x, screen_y, scale):
        """Converte coordenadas da tela para coordenadas do mundo"""
        # Converter de tela para mundo relativo
        relative_x = (screen_x - self.width / 2) * scale / self.zoom
        relative_y = (screen_y - self.height / 2) * scale / self.zoom
        
        # Aplicar offset da câmera
        world_x = relative_x + self.x
        world_y = relative_y + self.y
        
        return world_x, world_y
    
    def reset(self):
        """Reseta a câmera para a origem"""
        self.x = 0.0
        self.y = 0.0
        self.zoom = 1.0