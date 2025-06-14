import math

class CollisionSystem:
    def __init__(self, max_distance_from_sun=5e12):  # ~33 UA (além de Netuno)
        self.max_distance_from_sun = max_distance_from_sun
        self.sun_index = 0  # Assumindo que o Sol é sempre o primeiro corpo
        
    def check_collisions(self, bodies):
        """
        Verifica colisões entre corpos e retorna lista de corpos a serem removidos
        """
        bodies_to_remove = []
        
        for i in range(len(bodies)):
            for j in range(i + 1, len(bodies)):
                body1 = bodies[i]
                body2 = bodies[j]
                
                # Calcular distância entre os centros
                dx = body2.position[0] - body1.position[0]
                dy = body2.position[1] - body1.position[1]
                distance = math.sqrt(dx**2 + dy**2)
                
                # Converter raios de pixels para metros (aproximação)
                # Assumindo que 1 pixel ≈ 1e9 metros para esta simulação
                radius1_meters = body1.radius * 1e9
                radius2_meters = body2.radius * 1e9
                
                # Verificar colisão
                if distance < (radius1_meters + radius2_meters):
                    # Determinar qual corpo remover
                    if body1.mass < body2.mass:
                        if body1 not in bodies_to_remove:
                            bodies_to_remove.append(body1)
                    elif body2.mass < body1.mass:
                        if body2 not in bodies_to_remove:
                            bodies_to_remove.append(body2)
                    else:  # Massas iguais - remove ambos
                        if body1 not in bodies_to_remove:
                            bodies_to_remove.append(body1)
                        if body2 not in bodies_to_remove:
                            bodies_to_remove.append(body2)

        return bodies_to_remove
    
    def check_ejected_bodies(self, bodies):
        """
        Verifica corpos que foram ejetados do sistema solar (muito longe do Sol)
        """
        if len(bodies) == 0:
            return []
            
        ejected_bodies = []
        sun = bodies[self.sun_index]  # Primeiro corpo deve ser o Sol
        
        for i, body in enumerate(bodies):
            if i == self.sun_index:  # Não remover o Sol
                continue
                
            # Calcular distância do Sol
            dx = body.position[0] - sun.position[0]
            dy = body.position[1] - sun.position[1]
            distance_from_sun = math.sqrt(dx**2 + dy**2)
            
            if distance_from_sun > self.max_distance_from_sun:
                ejected_bodies.append(body)
        
        return ejected_bodies
    
    def merge_bodies(self, body1, body2):
        """
        Cria um novo corpo resultante da fusão de dois corpos (conservação de momento)
        Retorna o novo corpo ou None se não deve haver fusão
        """
        # Conservação de massa
        new_mass = body1.mass + body2.mass
        
        # Conservação de momento linear
        p1x = body1.mass * body1.velocity[0]
        p1y = body1.mass * body1.velocity[1]
        p2x = body2.mass * body2.velocity[0]
        p2y = body2.mass * body2.velocity[1]
        
        new_velocity = [
            (p1x + p2x) / new_mass,
            (p1y + p2y) / new_mass
        ]
        
        # Nova posição (centro de massa)
        new_position = [
            (body1.mass * body1.position[0] + body2.mass * body2.position[1]) / new_mass,
            (body1.mass * body1.position[1] + body2.mass * body2.position[1]) / new_mass
        ]
        
        # Novo raio baseado na massa combinada
        new_radius = max(body1.radius, body2.radius) + 2
        
        # Cor baseada no corpo mais massivo
        new_color = body1.color if body1.mass >= body2.mass else body2.color
        
        from physics.bodies import CelestialBody
        return CelestialBody(
            name=f"Fusão_{body1.name}_{body2.name}",
            mass=new_mass,
            position=new_position,
            velocity=new_velocity,
            radius=new_radius,
            color=new_color
        )