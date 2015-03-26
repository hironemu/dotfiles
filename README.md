### 最初にやること

#### git cloneで取ってくる

	$ git clone git://github.com/hironemu/dotfiles.git dotfiles

#### oh-my-zshを取得
	$ cd ~/dotfiles
	$ git submodule update --init 

#### oh-my-zshの設定とか

	$ ln -s ~/dotfiles/.zshrc ~/.zshrc 
	$ ln -s ~/dotfiles/.editrc ~/.editrc
	
#### .emacs.dのシンボリックリンクを作成

	$ ln -s ~/dotfiles/.emacs.d .emacs.d

#### Cask のインストール

	$ brew install cask

#### セットアップ

	$ cd ~/.emacs.d
	$ cask install
	$ emacs -Q -l setup.el

#### その他

##### Emacsの設定のバイトコンパイル

	C-u 0 M-x byte-recompile-directory
	ここでディレクトリを指定~/.emacs.d/elisps
	

[How do I byte-compile everything in my .emacs.d directory?](http://stackoverflow.com/questions/1217180/how-do-i-byte-compile-everything-in-my-emacs-d-directory)
