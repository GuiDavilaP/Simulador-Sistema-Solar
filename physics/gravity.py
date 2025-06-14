import math

G = 6.67430e-11  # Constante gravitacional (m³/kg⋅s²)

def calculate_gravitational_force(body1, body2):
    """
    Calcula a força gravitacional entre dois corpos
    Retorna a força aplicada EM body1 DEVIDO A body2
    """
    # Calcular vetor distância
    dx = body2.position[0] - body1.position[0]
    dy = body2.position[1] - body1.position[1]
    distance_squared = dx**2 + dy**2
    distance = math.sqrt(distance_squared)
    
    # Evitar divisão por zero ou distâncias muito pequenas
    # Usar o maior entre a distância calculada e um mínimo baseado nos raios
    min_distance = (body1.radius + body2.radius) * 1e9  # Converter pixels para metros
    if distance < min_distance:
        distance = min_distance
        distance_squared = distance**2
    
    # Calcular magnitude da força gravitacional
    force_magnitude = G * body1.mass * body2.mass / distance_squared
    
    # Componentes da força (unitário * magnitude)
    if distance > 0:
        force_x = force_magnitude * dx / distance
        force_y = force_magnitude * dy / distance
    else:
        force_x = force_y = 0
    
    return force_x, force_y