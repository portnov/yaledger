# Форматы файлов

YaLedger в общем случае может читать следующие файлы:

-   Общий конфигурационный файл `~/.config/yaledger/yaledger.yaml`.
    Описание параметров конфига см. [Config][].
-   Файл описания валют, по умолчанию
    `~/.config/yaledger/currencies.yaml`. Описание параметров см.
    [Currencies][].
-   Файл плана счетов, по умолчанию
    `~/.config/yaledger/default.accounts`. Описание параметров счетов
    см. [Accounts][].
-   Файл карты счетов, по умолчанию `~/.config/yaledger/default.map`.
    Описание формата см. [AccountsMap][]. Файл может быть пустым.
-   Собственно исходные файлы с записями о транзакциях. Поддерживаются
    следующие форматы:
    -   «Родной» формат файлов, `*.yaledger`. Описание синтаксиса см.
        [Syntax][].
    -   [CSV][]
    -   [HTML][]

-   Файлы `*.cbr` — используются для [загрузки курсов валют ЦБ РФ][CBRRates].

[Accounts]: Accounts.md
[AccountsMap]: AccountsMap.md
[CBRRates]: CBRRates.md
[Config]: Config.md
[CSV]: CSV.md
[Currencies]: Currencies.md
[HTML]: HTML.md
[Syntax]: Syntax.md
