from .bodies import CelestialBody
from .gravity import calculate_gravitational_force

DAY_IN_SECONDS = 86400  # Número de segundos em um dia
DEFAULT_DAYS_PER_SECOND = 10 

class SolarSystemSimulator:
    def __init__(self, days_per_second=DEFAULT_DAYS_PER_SECOND):
        self.bodies = []
        self.time_scale = DAY_IN_SECONDS * days_per_second  # Escala de tempo em segundos
        
    def add_body(self, body):
        self.bodies.append(body)
        
    def update(self, dt):
        """Atualiza o estado do sistema"""
        dt_scaled = dt * self.time_scale
        
        # Inicializa todas as forças como zero para cada corpo
        forces = {body: [0.0, 0.0] for body in self.bodies}
        
        # Calcula forças entre todos os pares de corpos
        for i, body1 in enumerate(self.bodies):
            for body2 in self.bodies[i+1:]:  # Evita duplicação e auto-força
                fx, fy = calculate_gravitational_force(body1, body2)
                
                # Aplica a 3ª Lei de Newton (ação e reação)
                forces[body1][0] += fx
                forces[body1][1] += fy
                forces[body2][0] -= fx  # Força oposta
                forces[body2][1] -= fy
        
        # Atualiza velocidades e posições
        for body in self.bodies:
            if body.mass == 0:
                continue  # Corpos com massa zero não aceleram
                
            ax = forces[body][0] / body.mass
            ay = forces[body][1] / body.mass
            
            body.velocity[0] += ax * dt_scaled
            body.velocity[1] += ay * dt_scaled
            body.update_position(dt_scaled)