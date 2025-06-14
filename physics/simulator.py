from .gravity import calculate_gravitational_force
from .collisions import CollisionSystem

DAY_IN_SECONDS = 86400
DEFAULT_DAYS_PER_SECOND = 10 

class SolarSystemSimulator:
    def __init__(self, days_per_second=DEFAULT_DAYS_PER_SECOND):
        self.bodies = []
        self.time_scale = DAY_IN_SECONDS * days_per_second
        self.collision_system = CollisionSystem()
        
        # Estatísticas para debug
        self.total_bodies_removed = 0
        self.collisions_detected = 0
        self.ejections_detected = 0
        
    def add_body(self, body):
        self.bodies.append(body)
        
    def remove_body(self, body):
        if body in self.bodies:
            self.bodies.remove(body)
            self.total_bodies_removed += 1
        
    def update(self, dt):
        """Atualiza o estado do sistema com detecção de colisões e remoções"""
        dt_scaled = dt * self.time_scale
        
        # Primeiro, atualiza a física
        self._update_physics(dt_scaled)
        
        # Depois, verifica colisões e remove corpos
        self._handle_collisions_and_ejections()
        
    def _update_physics(self, dt_scaled):
        """Atualiza apenas a física do sistema"""
        if len(self.bodies) < 2:
            return
            
        # Inicializa todas as forças como zero para cada corpo
        forces = {body: [0.0, 0.0] for body in self.bodies}
        
        # Calcula forças entre todos os pares de corpos
        for i, body1 in enumerate(self.bodies):
            for body2 in self.bodies[i+1:]:
                fx, fy = calculate_gravitational_force(body1, body2)
                
                # Aplica a 3ª Lei de Newton (ação e reação)
                forces[body1][0] += fx
                forces[body1][1] += fy
                forces[body2][0] -= fx
                forces[body2][1] -= fy
        
        # Atualiza velocidades e posições
        for body in self.bodies:
            if body.mass == 0:
                continue
                
            ax = forces[body][0] / body.mass
            ay = forces[body][1] / body.mass
            
            body.velocity[0] += ax * dt_scaled
            body.velocity[1] += ay * dt_scaled
            body.update_position(dt_scaled)
    
    def _handle_collisions_and_ejections(self):
        """Gerencia colisões e ejeções do sistema"""
        # Verificar colisões
        colliding_bodies = self.collision_system.check_collisions(self.bodies)
        
        # Verificar corpos ejetados
        ejected_bodies = self.collision_system.check_ejected_bodies(self.bodies)
        
        # Remover corpos colididos
        for body in colliding_bodies:
            self.remove_body(body)
            self.collisions_detected += 1
            
        # Remover corpos ejetados
        for body in ejected_bodies:
            self.remove_body(body)
            self.ejections_detected += 1
    
    def get_system_stats(self):
        """Retorna estatísticas do sistema para debug"""
        total_mass = sum(body.mass for body in self.bodies)
        total_kinetic_energy = sum(
            0.5 * body.mass * (body.velocity[0]**2 + body.velocity[1]**2) 
            for body in self.bodies
        )
        
        return {
            'total_bodies': len(self.bodies),
            'total_mass': total_mass,
            'total_kinetic_energy': total_kinetic_energy,
            'bodies_removed': self.total_bodies_removed,
            'collisions_detected': self.collisions_detected,
            'ejections_detected': self.ejections_detected
        }