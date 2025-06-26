from typing import Optional, Tuple

from .vectors import Vec2


class CelestialBody:
    __slots__ = ('_name', '_mass', '_position', '_velocity', '_radius', '_color')

    def __init__(self, name: str, mass: float, radius: float, 
                 position: Vec2, velocity: Vec2, 
                 color: Optional[Tuple[int, int, int]] = (0, 0, 0)):
        self.name = name
        self.mass = mass
        self.position = position
        self.radius = radius
        self.velocity = velocity
        self.color = color if color is not None else (0, 0, 0)

    @property
    def name(self) -> str:
        return self._name
    
    @name.setter
    def name(self, value: str):
        if not isinstance(value, str) or not value:
            raise TypeError("Name must be a non-empty string")
        if len(value) > 100:
            raise ValueError("Name cannot exceed 100 characters")
        self._name = value

    @property
    def mass(self) -> float:
        return self._mass
    
    @mass.setter
    def mass(self, value: float):
        try:
            value = float(value)
        except (TypeError, ValueError):
            raise ValueError("Mass must be a number")
        if value < 0:
            raise ValueError("Mass cannot be negative")
        self._mass = value

    @property
    def position(self) -> Vec2:
        return self._position
    
    @position.setter
    def position(self, value: Vec2):
        if not isinstance(value, Vec2):
            raise TypeError("Position must be a Vector2D instance")
        self._position = value

    @property
    def velocity(self) -> Vec2:
        return self._velocity
    
    @velocity.setter
    def velocity(self, value: Vec2):
        if not isinstance(value, Vec2):
            raise TypeError("Velocity must be a Vector2D instance")
        self._velocity = value

    @property
    def radius(self) -> float:
        return self._radius
    
    @radius.setter
    def radius(self, value: float):
        try:
            value = float(value)
        except (TypeError, ValueError):
            raise ValueError("Radius must be a number")
        if value < 0:
            raise ValueError("Radius cannot be negative")
        self._radius = value

    @property
    def color(self) -> tuple:
        return self._color
    
    @color.setter
    def color(self, value: tuple):
        if not isinstance(value, tuple) or len(value) != 3:
            raise TypeError("Color must be a tuple of RGB values")
        if any(not isinstance(c, int) or c < 0 or c > 255 for c in value):
            raise ValueError("Color values must be integers between 0 and 255")
        self._color = value

    def update_position(self, dt: float):
        """Atualiza a posição do corpo baseado na velocidade atual e no intervalo de tempo dt"""
        try:
            dt = float(dt)
        except (TypeError, ValueError):
            raise ValueError("dt must be a number")
        if dt < 0:
            raise ValueError("dt cannot be negative")
        self.position += self.velocity * dt

    def distance_to(self, other: 'CelestialBody') -> float:
        """Calcula a distância entre este corpo e outro"""
        if not isinstance(other, CelestialBody):
            raise TypeError("Other must be a CelestialBody instance")
        return self.position.distance_to(other.position)
    
    def check_collision(self, other: 'CelestialBody') -> bool:
        """Verifica se este corpo colide com outro baseado na distância e raios"""
        if not isinstance(other, CelestialBody):
            raise TypeError("Other must be a CelestialBody instance")
        return self.distance_to(other) <= (self.radius * 1e9 + other.radius * 1e9)
    
    def __eq__(self, other: object) -> bool:
        if not isinstance(other, CelestialBody):
            return False
        return (self.name == other.name and
                self.mass == other.mass and
                self.position == other.position and
                self.velocity == other.velocity and
                self.radius == other.radius and
                self.color == other.color)

    def __repr__(self):
        return (f"CelestialBody(name={self.name}, mass={self.mass}, "
                f"position=({self.position.x}, {self.position.y}), "
                f"velocity=({self.velocity.x}, {self.velocity.y}), "
                f"radius={self.radius}, color={self.color})")