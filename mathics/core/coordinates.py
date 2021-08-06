from numbers import Real


class Coordinates:
    def __init__(self, coordinates):
        self.coordinates = coordinates.to_python()

        if not all(2 <= len(coordinate) <= 3 for coordinate in self.coordinates):
            raise Exception

    @property
    def dimension(self):
        return len(self.coordinates[0])

    def __len__(self):
        return len(self.coordinates)
