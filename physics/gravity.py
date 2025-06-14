G = 6.67430e-11  # Constante gravitacional

def calculate_gravitational_force(body1, body2):
    """Calcula a força gravitacional entre dois corpos"""
    dx = body2.position[0] - body1.position[0]
    dy = body2.position[1] - body1.position[1]
    distance = (dx**2 + dy**2)**0.5
    
    # Evitar divisão por zero ou distâncias muito pequenas
    if distance < 1e-6:
        return 0, 0
    
    force_magnitude = G * body1.mass * body2.mass / (distance**2)
    force_x = force_magnitude * dx / distance
    force_y = force_magnitude * dy / distance
    
    return force_x, force_y