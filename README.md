# EZ Math Library

A comprehensive mathematical utility and vector library for the EZ language.

## Features

- **Vector Math**: Robust `Vector2` and `Vector3` models.
- **Statistics**: Built-in data analysis (mean, variance, stddev).
- **Trigonometry**: Accurate Taylor series approximations for `sin`, `cos`, and `tan`.
- **Utilities**: Interpolation (`lerp`) and constraints (`clamp`).

## Installation

```bash
ez install math
```

## Usage

```ez
use "math"
```

## API Reference

### Vector Models (`Vector2`, `Vector3`)
- `length()`: Calculates the magnitude.
- `normalize()`: Scales vector to unit length.
- `dot(v)`: Calculates the dot product with another vector.
- `print()`: Utility to output vector components.

### `Statistics` Model
- `mean()`: Arithmetic average.
- `variance()`: Data spread calculation.
- `stddev()`: Standard deviation.

### Trigonometry
- `PI()`: Returns π.
- `cos(x)` / `sin(x)` / `tan(x)`: Radians input.
- `cosd(d)` / `sind(d)` / `tand(d)`: Degrees input.
- `degToRad(d)` / `radToDeg(r)`: Conversion helpers.

## Examples

```ez
use "math"

// Vector operations
pos = Vector3(10, 0, 0)
dir = Vector3(1, 1, 0).normalize()
out "New pos: " + str(pos.dot(dir))

// Statistics
scores = [85, 92, 78, 95]
stats = Statistics(scores)
out "Class Average: " + str(stats.mean())

// Trig
angle = 45
out "Height: " + str(sind(angle) * 100)
```

