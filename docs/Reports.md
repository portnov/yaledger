# Отчёты

Результаты обработки транзакций выводятся в виде отчётов. Нужный отчёт
указывается в командной строке. Имя отчёта может быть сокращено до
первых нескольких букв, лишь бы сокращение было однозначным. У каждого
отчёта есть параметры, они указываются в командной строке после имени
отчёта. Кроме того, у отчёта могут быть опции (ключи командной строки);
опции указываются перед параметрами, но после имени отчёта например,
`yaledger balances -z доходы`.

Отчёты могут выдаваться по периодам времени, например, по дням, неделям.
Период определяется опцией командной строки `-p`, например,
`yaledger -p "2 weeks" registry`. Кроме того, есть
опции `--daily`, `--weekly`, `--monthly`, `--yearly`.

YaLedger в настоящий момент умеет следующие отчёты:

-   [balance][Balances] — текущие балансы по счетам.
-   [saldo][Saldo] — сальдо по счетам и группам счетов за период времени.
-   [registry][Registry] — список проводок по счёту или группе счетов.
-   [postings][Postings] — список полупроводок по счёту.
-   [details][Details] — список проводок по каждому из счетов группы.
-   [turnovers][Turnovers] — дебетовые и кредитовые обороты по счетам.
-   [incomestatement][IncomeStatement] — сводка о расходах и доходах.
-   [holds][HoldsReport] — данные об открытых холдах.
-   [stats][StatsReport] — статистика по счетам.
-   [flow][FlowReport] — сводная информация о потоке денег с одной группы счетов на другую.
-   [accounts][AccountsReport] — просто выводит список счетов.
-   [cat][Cat] — выводит все обработанные транзакции в формате,
    аналогичном «родному» входному формату.

Если отчёт не указан, то вызывается отчёт balance.

[AccountsReport]: AccountsReport.md
[Balances]: Balances.md
[Cat]: Cat.md
[Details]: Details.md
[FlowReport]: FlowReport.md
[HoldsReport]: HoldsReport.md
[IncomeStatement]: IncomeStatement.md
[Postings]: Postings.md
[Registry]: Registry.md
[Saldo]: Saldo.md
[StatsReport]: StatsReport.md
[Turnovers]: Turnovers.md
