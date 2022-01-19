import os
import pygame

from utils import scale, get_assets, get_mask, rotate

pygame.init()

FPS = 30

POSITIONS = (
    [(107, 670), (300, 670), (500, 670), (695, 670)],
    [(107, 481), (300, 481), (500, 481), (695, 481)],
    [(107, 291), (300, 291), (500, 291), (695, 291)],
    [(107, 107), (300, 107), (500, 107), (695, 107)],
)

PROLOG_PATH = os.path.join(os.path.dirname(
    __file__), os.pardir, 'prolog', 'main.pl')

ASSETS = os.path.join(os.path.dirname(
    __file__), os.pardir, 'python', 'assets')

FONT = os.path.join(ASSETS, 'Arial.ttf')

LIGHT = get_assets(ASSETS, 'light.png')[0]
LIGHT = scale(
    LIGHT,
    width=LIGHT.get_width()*0.55,
    height=LIGHT.get_height()*0.55
)

hunter_idle_path = os.path.join(ASSETS, 'hunter', 'idle')
HUNTER_IDLE = get_assets(hunter_idle_path, 'survivor-idle')
HUNTER_IDLE = list(
    map(
        lambda x: scale(x, width=x.get_width()*0.35,
                        height=x.get_height()*0.35),
        HUNTER_IDLE
    )
)

hunter_move_path = os.path.join(ASSETS, 'hunter', 'move')
HUNTER_MOVE = get_assets(hunter_move_path, 'survivor-move')
HUNTER_MOVE = list(
    map(
        lambda x: scale(x, width=x.get_width()*0.35,
                        height=x.get_height()*0.35),
        HUNTER_MOVE
    )
)

hunter_shoot_path = os.path.join(ASSETS, 'hunter', 'shoot')
HUNTER_SHOOT = get_assets(hunter_shoot_path, 'survivor-shoot')
HUNTER_SHOOT = list(
    map(
        lambda x: scale(x, width=x.get_width()*0.35,
                        height=x.get_height()*0.35),
        HUNTER_SHOOT
    )
)

warnings_path = os.path.join(ASSETS, 'warnings')
W_BREEZE = get_assets(warnings_path, 'breeze.png')[0]
W_BREEZE = scale(
    W_BREEZE,
    width=W_BREEZE.get_width() * 0.1,
    height=W_BREEZE.get_height()*0.1
)

W_STENCH = get_assets(warnings_path, 'stench.png')[0]
W_STENCH = scale(
    W_STENCH,
    width=W_STENCH.get_width() * 0.1,
    height=W_STENCH.get_height()*0.1
)

W_BS = get_assets(warnings_path, 'breeze-stench.png')[0]
W_BS = scale(
    W_BS,
    width=W_BS.get_width() * 0.1,
    height=W_BS.get_height()*0.1
)

W_GOLD = get_assets(warnings_path, 'gold.png')[0]
W_GOLD = scale(
    W_GOLD,
    width=W_GOLD.get_width() * 0.1,
    height=W_GOLD.get_height()*0.1
)

wumpus_idle_path = os.path.join(ASSETS, 'wumpus', 'idle')
WUMPUS_IDLE = get_assets(wumpus_idle_path, 'skeleton-idle')
WUMPUS_IDLE = list(
    map(
        lambda x: scale(x, width=x.get_width()*0.35,
                        height=x.get_height()*0.35),
        WUMPUS_IDLE
    )
)
WUMPUS_IDLE = list(
    map(
        lambda x: rotate(x, -90),
        WUMPUS_IDLE
    )
)

wumpus_blood_path = os.path.join(ASSETS, 'wumpus')
WUMPUS_BLOOD = get_assets(wumpus_blood_path, 'blood')


GOLD = get_assets(ASSETS, 'gold.png')[0]
GOLD = scale(GOLD, width=GOLD.get_width()*2, height=GOLD.get_height()*2)

PIT = get_assets(ASSETS, 'pit.png')[0]
PIT = scale(PIT, width=PIT.get_width()*2, height=PIT.get_height()*2)

MAP = get_assets(files='map')[0]
MAP = scale(MAP, width=MAP.get_width()*2.7, height=MAP.get_height()*2.7)

EXIT = get_assets(files='exit')[0]
EXIT = scale(EXIT, width=EXIT.get_width()*2.7, height=EXIT.get_height()*2.7)

WIDTH = MAP.get_width()
HEIGHT = MAP.get_height()

WIN = pygame.display.set_mode((WIDTH, HEIGHT))
pygame.display.set_caption('Wumpus World CLI Game Interface')
