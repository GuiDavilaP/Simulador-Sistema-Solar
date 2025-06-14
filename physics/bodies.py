class CelestialBody:
    def __init__(self, name, mass, position, velocity, radius, color):
        self.name = name
        self.mass = mass          # em kg
        self.position = position  # [x, y] em metros
        self.velocity = velocity  # [vx, vy] em m/s
        self.radius = radius      # para visualização (não afeta física)
        self.color = color        # para visualização
        
    def update_position(self, dt):
        """Atualiza posição baseada na velocidade atual"""
        self.position[0] += self.velocity[0] * dt
        self.position[1] += self.velocity[1] * dt