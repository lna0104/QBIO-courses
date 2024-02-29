#!/usr/bin/python3

# Simple pirate simulation using tkinter

import tkinter as tk 
from tkinter import font

import random
import os

class Pirate(object):
    """ The pirate."""

    def __init__(self, start, end, steps):
        self.start = start
        self.end = end
        self.dx = (end[0] - start[0]) // steps
        self.dy = (end[1] - start[1]) // steps
        self.x, self.y = start

    def step(self, coin):
        """The pirate takes a step.

        coin == 0 means a tail is thrown

        Return True iff still on plank

        Pirate.step(int) -> bool
        """

        if coin == 0:
            # tails
            if (self.x, self.y) != self.start:
                # not at beginning so step back
                self.x -= self.dx
                self.y -= self.dy
        else:
            # head - step forward
            self.x += self.dx
            self.y += self.dy
        return self.x >= self.end[0]

    def drop(self):
        """ The pirate drops into drink

        returns True iff in water

        Pirate.drop() -> None
        """
        self.y += 10
        if self.y > 1000:
            # reset pirate to beginning of plank
            self.x, self.y = self.start
            return True
        return False

    def get_position(self):
        """ Get the position for drawing the pirate image

        Pirate.get_position() -> (int, int)
        """
        return (self.x, self.y)


class PirateApp(object):
    # Screen size
    WIDTH = 700
    HEIGHT = 700

    # position of coin images
    COIN_X = 60
    COIN_Y = HEIGHT - COIN_X

    # background rectangle for count
    COUNT_RECT = [20, HEIGHT - 150, 100,  HEIGHT - 120]

    PLANK_START = (480, 440)      # the position to start the pirate
    PLANK_END = (290, 513)        # the end of the plank
    PLANK_STEPS = 5               # how many steps on the plank

    COIN_FLIP_DELAY = 40
    COIN_LAND_DELAY = 200
    COIN_FLIPS = 20

    """The application"""
    def __init__(self, master):
        master.title("Walk the plank")
        self._master = master
        self.count_font = font.Font(family="Helvetica", size=20)
        # The canvas on which to draw visualization
        self._canvas = tk.Canvas(master, width=self.WIDTH, height=self.HEIGHT)
        self._canvas.pack(expand=1, fill=tk.BOTH)
        # load the images from files
        self.load_images()
        
        self.flips = 0
        self.pirate = Pirate(self.PLANK_START, self.PLANK_END, self.PLANK_STEPS)
        self.count = 0
        self.show()
        self.step()

    def load_images(self):
        """
        Load images

        PirateApp.load_images() -> None
        """
        path = os.path.dirname(os.path.realpath(__file__))

        self.ship_image = tk.PhotoImage(file = path + '/data/ship.gif')
        self.pirate_image = tk.PhotoImage(file = path + '/data/pirate.gif')
        self.coin_images = [tk.PhotoImage(file = path + '/data/tail.gif'),
                            tk.PhotoImage(file = path + '/data/head.gif')]

    def step(self):
        """ Do a pirate step - flip coin and move

        PirateApp.step() -> None
        """
        # increment the number of coin tosses
        self.count += 1
        # toss coin: 0 is tail, 1 is head
        self.toss = random.randint(0,1)
        # number of flips of coin (just for coin animation)
        self.flips = self.COIN_FLIPS
        self.flip_coin()


    def flip_coin(self):
        """Animate spinning coin

        PirateApp.flip_coin() -> None
        """
        self.flips -= 1
        if self.flips != self.toss:
            # more flips to do
            self.show()
            # after delay turn coin over
            self._master.after(self.COIN_FLIP_DELAY, self.flip_coin)
        else:
            # flip animation finished - move pirate
            if self.pirate.step(self.toss):
                # pirate stays on board - flip a coin again
                self.show()
                self._master.after(500, self.step)
            else:
                # drop in drink
                self.drop_pirate()

    def drop_pirate(self):
        """Animate pirate dropping in drink

        PirateApp.drop_pirate() -> None
        """
        self.show()
        if self.pirate.drop():
            # pirate is in drink - reset and start again
            self.count = 0
            self._master.after(200, self.step)
        else:
            # more dropping
            self._master.after(12, self.drop_pirate)

    def show_counter(self):
        """Displays the flip counter.

        PirateApp.show_counter() -> None
        """
        # draw background for count display
        self._canvas.create_rectangle(self.COUNT_RECT, fill='blue', outline='blue')
        # draw count (as text)
        self._canvas.create_text(60, self.HEIGHT - 135,
                                 text="{0}".format(self.count),
                                 fill='red',font=self.count_font)

    def show(self):
        """Refresh screen, drawing current state

        PirateApp.show() -> None
        """
        # delete all graphic objects on canvas
        self._canvas.delete(tk.ALL)
        # redraw background
        self._canvas.create_image(self.WIDTH//2, self.HEIGHT//2, image=self.ship_image)
        # draw pirate in current position
        pirate_x, pirate_y = self.pirate.get_position()
        self._canvas.create_image(pirate_x, pirate_y, image=self.pirate_image)
        # draw current coin side
        self._canvas.create_image(self.COIN_X, self.COIN_Y,
                                  image=self.coin_images[self.flips % 2])
        self.show_counter()

def main():
    root = tk.Tk()
    app = PirateApp(root)
    root.mainloop()

if __name__ == '__main__':
    main()
