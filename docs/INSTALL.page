# Установка YaLedger

1. Установить компилятор GHC и необходимые библиотеки:

~~~
    $ sudo apt-get install haskell-platform git
~~~

2. Скачать исходники YaLedger:

~~~
    $ mkdir -p ~/src
    $ cd ~/src
    $ git clone git://gitorious.org/yaledger/yaledger.git
~~~

3. Скомпилировать и установить YaLedger:

~~~
    $ cd yaledger
    $ cabal update
    $ cabal install
~~~

4. По умолчанию команда `cabal install` положит исполнимый файл yaledger в
~/.cabal/bin. Чтобы запускать YaLedger, нужно либо скопировать этот
исполнимый файл в директорию, которая есть у вас в $PATH, либо добавить
директорию ~/.cabal/bin/ в PATH:

~~~
    $ cp ~/.cabal/bin/yaledger /usr/local/bin
    OR
    $ echo 'PATH=$PATH:~/.cabal/bin' >> ~/.bashrc
~~~

5. Создать настройки по умолчанию:

~~~
    $ yaledger init
~~~

6. Отредактировать файлы в `~/.config/yaledger/` по своему усмотрению.

