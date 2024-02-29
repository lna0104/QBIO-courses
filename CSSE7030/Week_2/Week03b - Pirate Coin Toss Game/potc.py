#!/usr/bin/python3

# Simple pirate simulation using tkinter - Pirates of the Caribbean

from pirate import *
import os

class POTCApp(PirateApp):
    # Screen size
    WIDTH = 1920
    HEIGHT = 799

    # position of coin images
    COIN_X = 60
    COIN_Y = HEIGHT - COIN_X

    # background rectangle for count
    COUNT_RECT = [20, HEIGHT - 150, 100, HEIGHT - 120]

    PLANK_START = (920, 455)  # the position to start the pirate
    PLANK_END = (400, 540)  # the end of the plank
    PLANK_STEPS = 5

    COIN_FLIP_DELAY = 50
    COIN_LAND_DELAY = 500
    COIN_FLIPS = 10

    def __init__(self, master):
        super().__init__(master)

        self.setup_audio()

    def load_images(self):
        """Load images

        POTCApp.load_images() -> None
        """
        path = os.path.dirname(os.path.realpath(__file__))

        ## Images from:
        ##     Pirates of the Caribbean: The Curse of the Black Pearl (c) 2003
        ##     Pirates of the Caribbean: Dead Man's Chest (c) 2006
        self.ship_image = tk.PhotoImage(file=path + "/potc/ship.gif")
        self.pirate_image = tk.PhotoImage(file=path + "/potc/sparrow.gif")
        self.coin_images = [tk.PhotoImage(file=path + "/potc/tail.gif"),
                            tk.PhotoImage(file=path + "/potc/head.gif")]

    def show_counter(self):
        """Displays the flip counter.

        POTCApp.show_counter() -> None
        """

        # draw background for count display
        self._canvas.create_rectangle(self.COUNT_RECT, fill='black', outline='gold')
        # draw count (as text)
        self._canvas.create_text(60, self.HEIGHT - 135,
                                 text="{0}".format(self.count),
                                 fill='gold', font=self.count_font)

    def _audio_toggle(self, ev=None):
        """Handles toggling audio playback.

        POTCApp._audio_toggle() -> None
        """
        if self._player.is_playing():
            text = "Play"
        else:
            text = "Pause"

        self._player.pause()
        self._audioMenu.entryconfigure(0, label=text)

    def setup_audio_menu(self):
        """Sets up user interaction with audio player.

        POTCApp.setup_audio_menu() -> None
        """
        menubar = tk.Menu(self._master)

        # create more pulldown menus
        self._audioMenu = menu = tk.Menu(menubar, tearoff=0)
        menu.add_command(label="Pause", command=self._audio_toggle)
        menubar.add_cascade(label="Audio", menu=menu)

        # display the menu
        self._master.config(menu=menubar)

        # bind to spacebar
        self._master.bind("<space>", self._audio_toggle)

    def setup_audio_player(self):
        """Sets up audio player.

        POTCApp.setup_audio_player() -> None
        """
        import vlc

        path = os.path.dirname(os.path.realpath(__file__))

        tracks = [
            "potc/Pirates of the Caribbean - Soundtrack 06 - Walk the Plank.mp3"
        ]

        media = vlc.MediaList()
        for track in tracks:
            media.add_media(path + "/" + track)

        player = vlc.MediaListPlayer()
        player.set_media_list(media)
        player.set_playback_mode(vlc.PlaybackMode.loop)
        player.play()

        self._player = player

    def setup_audio(self):
        """Sets up user interaction audio.

        POTCApp.setup_audio_menu() -> None
        """
        # Ignore OS Exception generated from no VLC found
        try:
            self.setup_audio_player()
            self.setup_audio_menu()
        except OSError:
            pass


def main():
    root = tk.Tk()
    app = POTCApp(root)
    root.mainloop()


if __name__ == '__main__':
    main()
