# Параметры командной строки

Список всех параметров выдаётся по `yaledger -h`:

    Usage: yaledger [OPTIONS] [REPORT] [REPORT PARAMS]
       or: yaledger init [DIRECTORY]
    Supported options are:
      -c FILE               --config=FILE                      Use specified config file instead of ~/.config/yaledger/yaledger.yaml
      -C FILE               --coa=FILE                         Chart of accounts file to use
      -M FILE               --map=FILE                         Accounts map file to use
      -r FILE               --currencies=FILE                  Currencies list file to use
      -f[FILE(s)]           --file[=FILE(s)]                   Input file[s]
      -s DATE               --start=DATE                       Process only transactions after this date
      -e DATE               --end=DATE                         Process only transactions before this date
      -S DATE               --report-from=DATE                 Start report from this date
      -E DATE               --report-to=DATE                   End report at this date
      -A                    --all-admin                        Process all admin records with any dates and attributes
      -a NAME=VALUE         --attribute=NAME=VALUE             Process only transactions with this attribute
      -P PERIOD             --period=PERIOD                    Output report by PERIOD
                            --daily                            Alias for --period "1 day"
                            --weekly                           Alias for --period "1 week"
                            --monthly                          Alias for --period "1 month"
                            --yearly                           Alias for --period "1 year"
      -d LEVEL              --debug=LEVEL                      Set debug level to LEVEL
      -p PARSER=CONFIGFILE  --parser-config=PARSER=CONFIGFILE  Use specified config file for this parser
      -h                    --help                             Show this help and exit

    Supported reports are: balances saldo registry postings details turnovers accounts cat holds incomestatement

Файлы с транзакциями задаются опцией ``-f``. Этих опций может быть
несколько — будет использовано несколько файлов. Можно задавать маски
файлов в стиле zsh — будут использованы все подходящие файлы. Опция ``-f``
без значения очищает список.

После всех опций указывается [отчёт][Reports], который нужно вывести, и
его параметры. По умолчанию выводится отчёт [balance][Balance].

Вместо отчёта может быть указана команда ``init``. В таком случае YaLedger устанавливает минимальный набор конфигов, необходимых для запуска. Эти конфиги могут быть использованы в качестве отправной точки. По умолчанию конфиги устанавливаются в директорию ``~/.config/yaleger/``. Другую директорию можно указать в качестве аргумента команде ``yaledger init``, например, ``yaledger init configs/``.

[Balance]: Balance.md
[Reports]: Reports.md
