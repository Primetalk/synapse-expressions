Система контактов SinapseGrid
=============================

Для интеграции компонентов системы в Речевом портале и продуктах на его основе применяется Система Контактов. Рассмотрим особенности функционирования систем реального времени и преимущества Системы Контактов.

Потребности систем реального времени
------------------------------------

Речевой портал является основой для разработки программных систем, которые должны работать в режиме реального времени. Такой режим работы характеризуется следующими свойствами:

    квантование и дискретизация непрерывных сигналов;
    порционная обработка данных, буферизация.

Первичная обработка непрерывных сигналов реального времени приводит к появлению дискретных событий, содержащих результаты обработки сигнала.

Квантование и дискретизация непрерывных сигналов
------------------------------------

При квантовании в каждый момент времени непрерывный сигнал с вещественными значениями округляется до ближайшего фиксированного уровня. При дискретизации значение сигнала записывается в дискретные моменты времени, а не непрерывно. Например, речевой сигнал квантуется до 16тыс. уровней и записывается с частотой дискретизации 16 кГц.

Буферизация (фреймы)
------------------------------------

Обрабатывать данные по одному отсчёту неэффективно — слишком большие накладные расходы на передачу отдельных отсчётов. Поэтому данные обрабатываются сразу большими порциями — фреймами. Для распознавания речи принято использовать фреймы размером 10 мс. Это связано с тем, что большинство звуков длится больше 10 мс и в пределах фрейма сигнал можно с определённой натяжкой считать стационарным. При частоте дискретизации 16 кГц за один раз обрабатывается 160 отсчётов.

Вторичная буферизация
------------------------------------

Для оценки медленно меняющихся параметров необходимо анализировать более длительные отрезки сигнала. Поэтому применяется вторичная буферизация. Например, для определения моментов начала и окончания речи необходимо анализировать сигнал в интервале сотен миллисекунд.

Потребности систем обработки речевого сигнала
------------------------------------

Речь представляет собой сложный сигнал, из которого может быть извлечена разнообразная информация:

*    моменты начала и окончания речи;
*    величина паузы после окончания речи;
*    громкость и отношение сигнал/шум речевого сигнала;
*    вектор признаков для модуля распознавания;
*    интонация и личностные особенности говорящего;
*    текст речи (в результате распознавания речи);
*    смысл фраз (в результате семантического и интонационного анализа);
*    намерение говорящего (в результате прагматического анализа);
*    и др.

Каждый вид извлекаемой информации становится доступным после проведения вычислений согласно определённому алгоритму обработки. Информация представляется в форме событий, которые появляются в момент завершения обработки. Алгоритмы обработки характеризуются

*    разветвлённостью обработки данных;
*    наличием обратных связей;
*    распределённостью состояния.

Разветвлённость обработки данных
------------------------------------

Многие виды извлекаемой информации вычисляются на основе промежуточных данных. При этом объём анализируемых данных (и длина буферизации) может отличаться в разных алгоритмах, и, следовательно, события генерируются в различные моменты времени. Поэтому алгоритмы обработки с одной стороны, имеют много общих элементов, а с другой стороны, содержат отличающиеся части, формирующие соответствующие события.

Система, вычисляющая сразу много видов информации, приобретает древообразный вид.

Дальнейшая обработка данных предполагает накопление и агрегирование разнородной информации, вычисляемой разными ветками системы. Тем самым, происходит слияние потоков обработки событий.

Для описания такого рода систем применяются DataFlow диаграммы.

Обратная связь
------------------------------------

Особым случаем обработки информации является наличие циклов на DataFlow диаграмме системы. Циклы соответствуют передаче информации в «обратную» сторону, то есть в алгоритм, генерирующий низкоуровневые события, передаётся информация, вычисленная на более поздних стадиях обработки. Например, если в ходе диалога удалось установить личностные особенности говорящего, акустическая модель может быть заменена на более подходящую. Или при переходе диалога к диктовке номера телефона языковая модель может быть специализирована. Или могут быть настроены веса языковой модели.

При классической иерархической декомпозиции систем (структурный подход, объектно-ориентированный подход) построение обратных связей существенно затруднено. Для передачи данных на 10 шагов назад необходимо в 10 промежуточных компонентах предусмотреть call-back-функции, принимающие данные обратной связи. В то же время, на DataFlow диаграмме такая передача изображается одной связью независимо от числа промежуточных компонентов.

Потребности сложных систем обработки событий
------------------------------------


Потребностями обработка входного речевого сигнала не исчерпываются все потребности системы ведения диалога. В состав модулей системы ведения диалога входят такие компоненты:

*    модули обмена аудио-данными с телефонным аппаратом собеседника;
*    модуль выделения речевого сигнала;
*    модуль распознавания речи;
*    анализатор динамики реплик сторон диалога (определение пауз, перебиваний);
*    модуль обработки телефонных событий (входящий звонок, перенаправление вызова, завершение разговора);
*    модуль синтеза речи;
*    модуль управления воспроизведением речи;
*    модуль многоканального плеера;
*    модуль ведения аудио и текстового журнала сессии;
*    и др.

Большинство этих модулей также описываются с помощью DataFlow диаграмм, содержащих обратные связи и разветвлённые алгоритмы. Более того, целые модули также могут быть связаны между собой обратными связями.

Наличие в составе сложной системы значительного количества модулей и подсистем приводит к необходимости их организации. Необходимо обеспечить изоляцию, инкапсуляцию и интеграцию (interconnection) модулей. Кроме того, так как многие алгоритмы обработки речевого сигнала требуют значительных вычислительных ресурсов, необходимо обеспечить возможность параллельного выполнения алгоритмов.

Ещё одним фактором, свойственным сложным системам, является живучесть при наличии ошибок. При возникновении ошибки в одном из вспомогательных модулей вся система должна иметь возможность продолжать функционирование (возможно, с временной потерей части функций). В то же время, при возникновении ошибок в критических модулях система должна завершить работу с освобождением всех ресурсов.

В ходе создания и эксплуатации сложных систем возникает необходимость их отладки, диагностики и внесения изменений.

Таким образом, в сложных системах обработки событий возникают следующие потребности:

*    организация модулей (изоляция, инкапсуляция, интеграция);
*    параллельность выполнения;
*    обработка ошибок.

Существующие аналоги и прототипы
------------------------------------

Так как разработка Речевого портала ведётся на платформе Java VM (преимущественно на языке Scala), то наибольший интерес представляют прототипы, имеющие версии для этой платформы.

Наиболее близким аналогом системе контактов является библиотека Akka (TypeSafe, http://akka.io/), основанная, в свою очередь, на идеях языка Erlang.

Концепция системы контактов
------------------------------------

Система Контактов основана на нескольких базовых принципах, которые будут рассмотрены в этом разделе.

Представьте себе макетную плату для сборки электронных схем. В этой плате подготовлены отверстия и контактные площадки. Некоторые контакты подписаны и расположены так, чтобы к ним было удобно подсоединять электронные приборы. Другие контакты служебные и просто используются для связи компонентов.

На макетной плате могут быть установлены компоненты, образующие, например, блок питания, или усилитель низкой частоты, или фильтр, или какую-нибудь ещё подсистему. Причём на такой плате могут остаться незадействованными часть контактов для подсистем, которые в собираемом устройстве не требуются. К ним не будут подсоединены никакие компоненты. Некоторые выводы блока питания могут оказаться невостребованными. Или часть входов универсальной микросхемы могут остаться неиспользованными.

Метафора макетной платы может служить неплохой иллюстрацией к системе контактов.

Контактом называется объект типа Contact[T], имеющий имя, обычно совпадающее с именем переменной, в которой хранится ссылка на него. Пример (все примеры на языке Scala):

<pre>
	val myContact = contact[String]("myContact")
</pre>

Контакт не содержит данных, он только обозначает точку в макетной плате. К нему могут быть подсоединены входы или выходы компонентов, имеющих тип String.

Двухполюсным компонентом или "стрелочкой" является компонент, имеющий один вход и один выход. В простейшем и наиболее широко применяемом случае в качестве компонента может использоваться обычная функция

<pre>
	def getLength(s:String) = s.length
</pre>

Запишем теперь конструкцию, которая для каждой поступающей строки будет вычислять её длину:

<pre>
	val len = contact[Int]("len")
	myContact -> len map getLength
</pre>

или, более коротко,

<pre>
	val len = myContact.map(_.length)
</pre>

![example1 system picture](images/example1.png)

(В последнем случае контакт len будет создан автоматически и соответствующего типа (Int).)

Обработка данных
------------------------------------

Вышеприведённый код сам по себе не выполняет никакой работы. В контактах данные не хранятся, в функциях тоже не хранятся. Этот код только описывает структуру системы — какие контакты с какими компонентами связаны.

Для того, чтобы связать данные с каким-либо контактом, используется «внешнее связывание». То есть создаётся объект, содержащий ссылку как на контакт, так и на данные, связанные с этим контактом. Такой объект в терминологии Системы Контактов называется Сигналом.

<pre>
	case class Signal[T](contact:Contact[T], data:T)
</pre>

(Кроме термина Сигнал можно встретить термины Событие, Данные, Фрейм, Сообщение.)

Состояние системы представляется списком сигналов на разных контактах в один дискретный момент времени.

<pre>
	type Signals = List[Signal[_]]
</pre>

Реализован специальный компонент SignalProcessor, который выполняет функциональное преобразование исходного списка сигналов в один момент времени в список сигналов последующий момент времени. Каждый сигнал по очереди передаётся на вход каждого компонента, подключенного к соответствующему контакту. Результат работы компонента в форме сигнала (или нескольких сигналов) добавляется к списку сигналов следующего момента времени. Когда все сигналы предшествующего момента времени обработаны, SignalProcessor завершает работу.

В теории скрытых марковских моделей есть хорошее понятие треллис (trellis). Это развёртка во времени совокупности сигналов. И SignalProcessor как раз  и используется для построения trellis'а на основе входных данных.

В какой момент останавливается построение trellis'а? Если обработку никак не останавливать, то все данные дойдут до крайних контактов и, т.к. там не подключено никаких компонентов, то данные исчезнут. Чтобы этого избежать, в описании системы указывается, какие контакты являются выходными

<pre>
	outputs(len) // outputs(output1, output2, output3)
</pre>

поэтому обработка останавливается тогда, все сигналы в текущем списке принадлежат множеству выходных контактов.

Типы стрелочек
------------------------------------

При обработке данных часто возникает ситуация, когда на один входной элемент генерируется 0 и больше выходных элементов. Обработка такой ситуации в языке Scala осуществляется с помощью функции высшего порядка flatMap. Поэтому в системе контактов стрелочки, аннотированные функциями, возвращающими 0..n элементов, имеют тип FlatMap.

<pre>
	val wordsContact = someStringContact.flatMap(_.split("\\s+".r))
<pre>

![example2 system picture](images/example2.png)

Важным частным случаем стрелочек типа FlatMap являются стрелочки 0..1, пропускающие или не пропускающие данные в зависимости от некоторых условий. Предусмотрен специальный метод для создания таких стрелочек — filter:

<pre>
	val nonEmptyString = myContact.filter(_.length>0)
</pre>

For-comprehension совместимость
-------------------------------

Интересной особенностью языка Scala является возможность использования syntactic sugar для пользовательских методов. В частности, так как в системе контактов объявлены методы map, flatMap, filter и withFilter, появляется возможность использовать for-comprehension:

<pre>
	val helloContact = for {
	   s <- myContact
	   if s.length >0
	} yield "Hello, "+s
</pre>

Этот код эквивалентен цепочке, состоящей из двух стрелочек:

<pre>
	val helloContact = myContact.filter(s => s.length>0).map(s=>"Hello, "+s)
</pre>

В некоторых случаях, когда алгоритм обработки разветвляется не очень сильно, такой синтаксис выглядит неплохо.

Работа с состоянием
-------------------

До сих пор все примеры оперировали только данными, приходящими на входной контакт. Результат нигде не сохранялся и передавался далее. То есть использовались "чистые" функции без побочных эффектов — immutable. Такие функции обладают массой полезных свойств. Например, легко распараллелить обработку на несколько потоков. Не требуется пересоздавать систему для обработки других данных — достаточно один раз при старте приложения её создать. Отладка таких систем практически исключена за ненадобностью — из-за отсутствия внутреннего состояния и побочных эффектов результат всегда детерминированно определяется входными данными.

Если логика обработки данных требует сохранения состояния, то первое, что приходит в голову — использовать внутри функции переменную и сохранять состояние в ней. К примеру, так:

<pre>
	var counter = 0
	val helloCount = myContact.map({any => 	counter += 1;  counter})
</pre>

Этот способ будет работать, но, к сожалению, мы теряем все преимущества immutable системы.

А что, если хранить состояние отдельно от системы? И в нужный момент перед работой функции текущее состояние извлекается, а потом помещается обратно.

Как работать с таким состоянием, которое где-то хранится? Функция должна принимать на вход текущее значение состояния и возвращать новое значение.

<pre>
	val helloCount = myContact.[указание на переменную, где хранится состояние counter].map({(any, counter) => (counter+1, counter + 1)})
</pre>

Давайте более внимательно посмотрим на эту функцию. Запишем её verbose через def:

<pre>
	def incCounter(any:String, counter:Int) : (Int, Int) = {
	  val newCounterValue = counter+1
	  val resultOfThisFunction = newCounterValue
	  return (resultOfThisFunction, newCounterValue)
	}
</pre>

Функция, обрабатывающая состояние, — чистая. Ч.т.д.

Остаётся только определиться, как нам ловко хранить и извлекать состояние.

Для идентификации различных переменных состояния мы будем использовать разновидность контакта — StateHandle[T].

<pre>
	val counterS = state[Int]("counterS", 0)
	val helloCount = contact[Int]("helloCount")
</pre>

Такой идентификатор содержит название переменной, тип и начальное значение. (TODO: сделать макрос вида: state counterS:Int = 0)

Текущее значение состояния при объявлении недоступно. Оно нигде не хранится. (Забегая немного вперёд: SignalProcessor хранит текущие значения всех переменных состояния в Map'е).

Чтобы в нашей функции helloCounter использовать это состояние, необходимо на него сослаться:

<pre>
    (myContact.withState(counterS) -> helloCount).stateMap({(counter: Int, any:String) => (counter + 1, counter + 1)},"inc "+counterS)
	val helloCount = myContact.stateMap(counterS, {(any, counter) => (counter+1, counter + 1)})
</pre>

В итоге получилось несколько громоздко, но зато мы имеем все преимущества чистых функций.

![example3 system picture](images/example3.png)

В DSL имеется набор вспомогательных функций высшего порядка, упрощающих работу с состояниями.

Рисование схемы системы
-----------------------

Так как наша система описана полностью декларативно, сразу же появляется возможность её анализа. В частности, очень удобно смотреть на граф системы.

Чтобы получить изображение системы, достаточно вызвать метод toDot. В этом методе производится обход всех элементов системы (контактов, стрелочек, подсистем) и формируется текстовый файл в формате dot. Просмотреть такой файл можно с помощью, например, программы XDot.

Конструирование системы с помощью Builder'ов
--------------------------------------------

Все примеры создания контактов и стрелочек должны находиться в каком-нибудь классе/трейте, унаследованном от SystemBuilder. Именно в нём находятся основные методы, позволяющие инкрементно создавать контакты и разнообразные стрелочки. Сам SystemBuilder, как подсказывает его название, является mutable классом и не участвует непосредственно в runtime-обработке. Чтобы получить чистое описание системы, построенное Builder'ом, достаточно вызвать метод toStaticSystem. Этот метод возвращает простой immutable case-класс, содержащий все контакты и стрелочки.

Некоторые специальные DSL находятся в отдельных трейтах, которые надо просто подключить к своему Builder'у, чтобы этим DSL воспользоваться.

Для конструирования системы кроме очевидного способа

<pre>
	val sb = new SystemBuilderC("MySystem")
	import sb._
	...
	val system = sb.toStaticSystem
</pre>

можно пользоваться также и наследованием trait'а:

<pre>
	trait MySystemBuilder extends SystemBuilder {
	  // setSystemName("MySystem") 
	  ...
	}

	val system = new MySystemBuilder.toStaticSystem
</pre>

После получения StaticSystem, её можно непосредственно использовать в SignalProcessor'е для обработки сигналов. При этом состояние системы придётся всё время передавать на вход SignalProcessor'у и запоминать при возврате. Чтобы упростить управление состоянием, имеется специальный класс DynamicSystem = StaticSystem + State. Пользоваться таким классом можно как обычной функцией (правда, имея в виду, что внутри спрятано состояние и функция имеет побочный эффекти).

Подсистемы
----------

По мере увеличения программы, написанной на контактах, возникает необходимость выделения блоков в подсистемы для целей повторного использования. Чтобы добавить подсистему, используется метод addSubsystem. Так как у подсистемы имеется своё состояние, то также указывается stateHandle, где будет хранится состояние.

<pre>
	val subsystem = new MySubsystemBuilder.toStaticSystem
	val s1 = state[SystemState]("s1", subsystem.s0)
	sb.addSubsystem(subsystem, s1)
</pre>

Чтобы подсистема получала входные данные, некоторые её контакты должны быть объявлены как входные:

<pre>
	inputs(input1, input2)
</pre>

в этом случае данные, появляющиеся во внешней системе на соответствующих контактах, будут обработаны подсистемой.

Если требуется подключить несколько экземпляров подсистемы, то хотелось бы, чтобы эти экземпляры могли быть привязаны к разным входным и выходным контактам. Для этого используются подсистема, вложенная в подсистему. В промежуточной подсистеме увязываются входы со входами и выходы с выходами. Для этого в builder'е промежуточной подсистемы используются методы mappedInput, mappedOutput, inputMappedTo, mapToOutput. Эти методы обеспечивают создание wiring'ов, обеспечивающих связи между контактами внешней подсистемы и контактами внутренней подсистемы.

Использование Akka Actor'ов
---------------------------

Описанные системы работают в одном потоке. Один из возможных способов перехода к многопоточности заключается в том, чтобы превратить целую систему в Actor, который уже будет совместим с параллельностью Akka.

Если на вход Actor'а поступает Signal, то его обработка выполняется очевидным образом — сам сигнал передаётся во вложенную DynamicSystem. Для совместимости с программами, не работающими с сигналами, используется специальный контакт NonSignalWithSenderInput. Этот контакт имеет тип (ActorRef, Any). Первый элемент будет содержать sender поступивших данных, а второй элемент — собственно данные.


