# Загрузка курсов валют ЦБ РФ

Данный модуль является скорее примером возможного применения механизма
парсеров YaLedger, но может также быть использован в практических целях.

В конфигурационном файле (по умолчанию `~/.config/yaledger/cbr.yaml`)
указывается, какие валюты необходимо загружать и с какой периодичностью.
Конфигурационный файл представляет собой список словарей со следующими
ключами:

-   currency. Валюта, например, “$”.
-   code. Числовой код валюты, например, 840.
-   reversible. Считать ли загружаемые курсы данной валюты обратимыми.
-   start-from. Начальная дата, с которой загружать курсы.
-   interval. Период загрузки курсов, например, “2 days”. По умолчанию —
    “1 week”.

Файлы с именами `*.cbr` используются в качестве кэша, чтобы не загружать
дважды одни и те же курсы. Курсы хранятся в обычном формате транзакций
YaLedger. Файл `*.cbr` может быть и пустым. Таким образом, чтобы только
загрузить курсы, указанные в конфиге, достаточно дать команды

    $ touch rates.cbr
    $ yaledger -f -frates.cbr cat

Курсы будут выведены на stdout и записаны в rates.cbr.
