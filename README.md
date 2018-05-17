# sclogistic
We make model of logistic company 


## План: 

1) Для начала разберитесь с гитом, как коммитить в гитхаб и попробуйте создать репозиторий и поделать с ним что-нибудь, понятно, что у меня локальная копия всегда будет на компе, но все-таки, так вы быстрее разберетесь, хотя если хотите, можете попробовать и на этом репозитории

2) Потом каждому определим по заданию и будем их делать, задания будут не большие, специально, чтобы мы быстро их делали и видели прогресс и проделанную работу

## Идеи: 

1) Можно задавать города координатами, чтобы потом вычислять расстояния, да и вообще все точки отечать координатами, думаю это было бы удобно + к каждой деревушке сможем знать маршрут

2) Интересно как хранить всю эту информацию, по хорошему бы подключить какую-нибудь БД, типа mysql, но для начала думаю и текстовый файлик сойдет( как раз поработаем с файлами)

3) Думаю вывод тоже делать в файл, а также наверное стоит считать время, и количество итераций, для того, чтобы понимать как все работает

4) Также надо не забывать все делать через параметры, так что никаких констант в коде, все должно регулироваться из файла, так мы добьемся нашей пресловутой маштабируемости

5) Для начала наверное нужно просто создать граф, на котором у нас будут расстояния, и мы будем вычислять расстояния до городов по широте и долготе, а потом считать оптимальный путь, по заданному маршруту

6) Ну а дальше уже можно вводить такое понятие, как время, которое будет зависит от многих факторов, пока что будем задавать эти факторы параметрами

7) Дальнейшим развитием вижу разделить программу на 2 части: ядро, которое будет просчитывать оптимальность путей, время и принимать решение, и внешняя оболочка, которая будет задавать различные параметры и генерировать случайные события( аварии, проблемы с двигателем, пробки), которые тоже будут влиять на маршрут, и ядро должно будет перестраиваться под эти задачи. Для соединения их между собой стоит использовать что-нибудь типа kafka streams прикольная штука, я уже частично разобрался, так что наверное этим буду заниматься я. 

8) Потом уже можем добавить какой-нибудь google maps api, с помощью которого мы сможем перенести нашу систему в реальный мир, с реальными дорогами, пробками, так что нам больше не прийдется задавать маршруты и дороги, мы будем указывать только начальную и конечную точку, а ядро уже будет вычислять все само. 

P.S. Думаю пока достаточно накидал, если мы это все сделаем к концу лета, то уже будет круто, ведь куда не копни, сразу будут вылезать всякие сложности
lol
