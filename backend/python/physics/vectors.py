import math

from .physical_constants import EPS


class Vec2:
    __slots__ = ('_x', '_y')

    def __init__(self, x: float = 0.0, y: float = 0.0):
        self.x = x
        self.y = y

    @property
    def x(self) -> float:
        return self._x
    
    @x.setter
    def x(self, value: float):
        try:
            value = float(value)
        except (TypeError, ValueError):
            raise ValueError("x must be a number")
        self._x = value

    @property
    def y(self) -> float:
        return self._y
    
    @y.setter
    def y(self, value: float):
        try:
            value = float(value)
        except (TypeError, ValueError):
            raise ValueError("y must be a number")
        self._y = value

    def length(self) -> float:
        """Calcula o comprimento do vetor"""
        return math.sqrt(self.x ** 2 + self.y ** 2)
    
    def normalize(self) -> 'Vec2':
        length = self.length()
        if length < EPS:
            raise ValueError("Cannot normalize a zero-length vector")
        return Vec2(self.x / length, self.y / length)
    
    def dot(self, other: 'Vec2') -> float:
        return self.x * other.x + self.y * other.y
    
    def cross(self, other: 'Vec2') -> float:
        return self.x * other.y - self.y * other.x
    
    def distance_to(self, other: 'Vec2') -> float:
        if not isinstance(other, Vec2):
            raise TypeError("Can only calculate distance to another Vec2")
        return math.sqrt((self.x - other.x) ** 2 + (self.y - other.y) ** 2)
    
    def as_tuple(self) -> tuple[float, float]:
        return (self.x, self.y)

    def __hash__(self) -> int:
        return hash((self.x, self.y))

    def __eq__(self, other: 'Vec2') -> bool:
        if not isinstance(other, Vec2):
            return False
        return abs(self.x - other.x) < EPS and abs(self.y - other.y) < EPS

    def __add__(self, other: 'Vec2') -> 'Vec2':
        if not isinstance(other, Vec2):
            raise TypeError("Can only add Vec2 to Vec2")
        return Vec2(self.x + other.x, self.y + other.y)

    def __sub__(self, other: 'Vec2') -> 'Vec2':
        if not isinstance(other, Vec2):
            raise TypeError("Can only subtract Vec2 from Vec2")
        return Vec2(self.x - other.x, self.y - other.y)

    def __mul__(self, scalar: float) -> 'Vec2':
        if not isinstance(scalar, (int, float)):
            raise TypeError("Scalar must be a number")
        return Vec2(self.x * scalar, self.y * scalar)
    
    def __rmul__(self, scalar: float) -> 'Vec2':
        return self.__mul__(scalar)

    def __truediv__(self, scalar: float) -> 'Vec2':
        if not isinstance(scalar, (int, float)):
            raise TypeError("Scalar must be a number")
        if abs(scalar) < EPS:
            raise ValueError("Cannot divide by zero")
        return Vec2(self.x / scalar, self.y / scalar)
    
    def __neg__(self) -> 'Vec2':
        return Vec2(-self.x, -self.y)

    def __abs__(self) -> float:
        return self.length()

    def __repr__(self) -> str:
        return f"Vec2({self.x}, {self.y})"
    