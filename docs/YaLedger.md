# YaLedger

YaLedger разрабатывается как «полновесное», full-featured приложение для
«домашней бухгалтерии» в стиле ledger / hledger. В отличие от них,
yaledger использует полновесную схему бухучёта, основанную на
транзакциях, проводках и полупроводках. Это позволяет реализовать
возможности, характерные для коммерческих бухгалтерских программ.

Описания версий: [0.1.0.0][README], [0.1.1.0][0110], [0.1.2.0][0120],
[0.1.3.0][0130]

-   [Общее описание][README]
-   [Установка][INSTALL]
-   [Используемые принципы бухгалтерского учёта][Accounting]
-   [Общий алгоритм работы][Algo]
-   [Параметры командной строки][CmdLine]
-   [Форматы входных файлов][Files]:
    -   [Глобальный конфигурационный файл][Config]
    -   [Файл описания валют][Currencies]
    -   [План счетов][Accounts]
    -   [Карта счетов][AccountsMap]
    -   [«Родной» формат журнала транзакций][Syntax]
    -   [«Табличные» форматы][Tables]:
        -   [CSV][]
        -   [HTML][]

    -   [Загрузка курсов валют ЦБ РФ][CBRRates]

-   Возможности YaLedger:
    -   [Атрибуты счетов и транзакций][Attributes]
    -   [Автоматический поиск корреспондирующего счёта][Correspondence]
    -   [Сверка балансов счетов][Reconciliation]
    -   [Шаблоны транзакций][Templates]
    -   [Периодические транзакции][Periodic]
    -   [Автоматическое создание транзакций по правилам][Rules]
    -   [Определение дублированных транзакций][Deduplication]
    -   [Поддержка резервов счетов][Holds]
    -   [Проверка балансов счетов][BalanceChecks]
    -   [Автоматическое перенаправление части дебета][DebitRedirect]

-   [Отчёты][Reports]:
    -   [Balances][]
    -   [Saldo][]
    -   [Registry][]
    -   [Postings][]
    -   [Details][]
    -   [IncomeStatement][]
    -   [turnovers][Turnovers]
    -   [holds][HoldsReport]
    -   [stats][StatsReport]
    -   [flow][FlowReport]
    -   [accounts][AccountsReport]
    -   [cat][Cat]

-   Приложения:
    -   [Возможные варианты учёта транзакций по банковским картам с использованием банковских выписок по карте][CardExamples]
    -   [Возможные варианты управления будущими тратами][FutureExpenses]
    -   [Графики][Charts]

[0110]: 0110.md
[0120]: 0120.md
[0130]: 0130.md
[Accounting]: Accounting.md
[Accounts]: Accounts.md
[AccountsMap]: AccountsMap.md
[AccountsReport]: AccountsReport.md
[Algo]: Algo.md
[Attributes]: Attributes.md
[BalanceChecks]: BalanceChecks.md
[Balances]: Balances.md
[CardExamples]: CardExamples.md
[Cat]: Cat.md
[CBRRates]: CBRRates.md
[Charts]: Charts.md
[CmdLine]: CmdLine.md
[Config]: Config.md
[Correspondence]: Correspondence.md
[CSV]: CSV.md
[Currencies]: Currencies.md
[DebitRedirect]: DebitRedirect.md
[Deduplication]: Deduplication.md
[Details]: Details.md
[Files]: Files.md
[FlowReport]: FlowReport.md
[FutureExpenses]: FutureExpenses.md
[Holds]: Holds.md
[HoldsReport]: HoldsReport.md
[HTML]: HTML.md
[IncomeStatement]: IncomeStatement.md
[INSTALL]: INSTALL.md
[Periodic]: Periodic.md
[Postings]: Postings.md
[README]: ../README.md
[Reconciliation]: Reconciliation.md
[Registry]: Registry.md
[Reports]: Reports.md
[Rules]: Rules.md
[Saldo]: Saldo.md
[StatsReport]: StatsReport.md
[Syntax]: Syntax.md
[Tables]: Tables.md
[Templates]: Templates.md
[Turnovers]: Turnovers.md
