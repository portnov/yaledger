# Отчёт postings — полупроводки по одному счёту или всем счетам группы

## Описание

Отчёт выводит все полупроводки по указанному счёту за отчётный период.
Если указана группа счетов — отчёт строится отдельно по каждому счёту в
группе.

## Параметры

Один необязательный параметр — путь к счёту или группе счетов. Если не
указан, отчёт строится по всем счетам.

## Примеры вызова

    $ yaledger postings X1
    WARNING: Balance of Z5 will be 0.0000000000$
    From the begining till 2012/10/06
    /X1:
        DATE    |  AMOUNT  
    ============|==========
     2012/09/09 | 81.0000€ 
     2012/09/10 | 16.2000€ 
     2012/09/11 |  0.0723€ 
     2012/09/16 |  1.6200€ 
     2012/09/17 |  3.0000€ 
     2012/09/23 | 32.4000€ 
