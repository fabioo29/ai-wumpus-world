import os
import pygame

ASSETS = os.path.join(os.path.dirname(
    __file__), os.pardir, 'python', 'assets')


def scale(image, width: int = 30, height: int = 15):
    """Scale the image to the specified width and height."""
    image = pygame.transform.scale(image, (width, height))
    return image


def rotate(image, angle):
    """Rotate the image to the specified angle."""
    image = pygame.transform.rotate(image, angle)
    return image


def get_assets(folder: str = None, files: str = ''):
    """get all assets with name starting with files... from the specified folder"""
    if folder:
        assets_path = os.path.join(ASSETS, folder)
    else:
        assets_path = ASSETS

    assets_files = os.listdir(assets_path)
    assets_files = sorted(assets_files, key=lambda x: (len(x), x))

    assets = []
    for file in assets_files:
        if files in file:
            elem = pygame.image.load(os.path.join(assets_path, file))
            assets.append(elem)
    return assets
