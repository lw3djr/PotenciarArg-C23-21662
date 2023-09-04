// Menu para  proyecto final Potenciar Argentina
// funcion para centrar texto
Funci�n centrar(TextPantalla,inicial)
	Definir i Como Entero
	Para i<-0 Hasta (inicial-(Longitud(TextPantalla)/2)) Hacer
		Escribir ' 'Sin Saltar
	FinPara
	Escribir TextPantalla
FinFunci�n

// SubProceso para asignar numero afiliado
Funci�n reserv <- CalcularNumAfiliado
	reserv <- azar(9999)+1
	// escribir afiliado
	// retorna el numero de afiliado
FinFunci�n

// Sub Proceso que muestra los 10, secciones que el usuario visito durante la ejecucion
Funci�n HistorialMuestra(arreglo)
	Escribir ' '
	Escribir ' Aqui le presentamos las 10 secciones que visit�'
	Escribir ' '
	Escribir ' Si visit� menos de 10 el resto aparecera sin datos'
	Escribir ' '
	Para inicio<-1 Hasta 10 Con Paso 1 Hacer
		Escribir ' --> ', inicio, ' : ', arreglo[inicio]
	FinPara
FinFunci�n

// Sub Proceso que cargaria el Historial
Funci�n HistorialCarga(vector,cantidad,lugar)
	Si cantidad<=10 Entonces
		vector[cantidad] <- lugar
	FinSi
FinFunci�n

// seria la firma de cada salida o regreso, una especie de pie de pagina
Funci�n datosTurismo(vienede)
	// funcion para listar datos de contacto TURISMO
	Escribir ' Gracias por visitar la secci�n ', vienede
	Escribir ''
	Escribir ' Nuestros datos de contacto:'
	Escribir '                          Turismo Social - Legajo 9518'
	Escribir '                           Calle 5 N� 1657 - La Plata'
	Escribir 'Tel./Fax.: (0221) 425-6666 / 427-1230 / L�nea gratuita: 0800-888-5784 - Interno. 200'
	Escribir '	                              WhatsApp: 221599999'
	Escribir '                     Correo electr�nico: turismo@tugremio.org '
	Escribir '	              Horario de atenci�n: lunes a viernes de 8:30 a 15.30 hs. '
FinFunci�n

// INICIO PROGRAMA PRINCIPAL 
Algoritmo menuObraSocial
	// Variables numericas
	Definir afiliadoPleno Como Entero
	Definir afiliadoAdherente Como Entero
	Definir adicionalConyuge Como Entero
	Definir adicionalHijos Como Entero
	Definir estimador Como Entero
	Definir valorFinalCuota Como Entero
	Definir tipoAfiliado Como Entero
	Definir OP Como Entero
	Definir AfiliadoTempora Como Entero
	Definir afiliado Como Entero
	Definir porcentajeReintegro Como Real
	Definir practicas Como Entero
	Definir montoReintegro Como Real
	Definir vueltas Como Entero
	Definir valorReintegroOdontologico Como Real
	Definir odontologicos Como Entero
	Definir montoOdontologico Como Real
	Definir valorReintegroortopedia Como Real
	Definir facturasOrtoped Como Entero
	Definir facturaValor Como Entero
	Definir monto Como Entero
	Definir montoTotalReintegros Como Real
	Definir simulador Como Entero
	Definir continuar Como Entero
	Definir cantidadHijos Como Entero
	Definir contador Como Entero
	Definir MiniTurismoOpcion Como Entero
	Definir numeroTelefonico Como Entero
	Definir reservaGasto Como Entero
	// varaibles caracter
	Definir OPturismo Como Cadena
	Definir Eleccion Como Cadena
	Definir Encabezado Como Cadena
	Definir seccion Como Cadena
	Definir destino Como Cadena
	Definir muchasGracias Como Cadena
	// varaibles logicas
	Definir pasoPorReintegros Como L�gico
	Definir pasoPorTurismo Como L�gico
	Definir pasoPorDescuentos Como L�gico
	Definir pasoPorEstimadorCuota Como L�gico
	Definir pasoPorAfiliaciones Como L�gico
	// variables vectores
	Dimensionar Historia(10)
	// inicializador de variables generales
	afiliadoPleno <- 7500
	afiliadoAdherente <- 5000
	adicionalConyuge <- 3000
	adicionalHijos <- 2000
	afiliado <- CalcularNumAfiliado()
	pasoPorReintegros <- Falso
	pasoPorTurismo <- Falso
	pasoPorDescuentos <- Falso
	pasoPorEstimadorCuota <- Falso
	pasoPorAfiliaciones <- Falso
	Repetir
		// mostrar menu General
		Limpiar Pantalla
		Encabezado <- 'Men� de Prestaciones y beneficios'
		centrar(Encabezado,40)
		Escribir ''
		Escribir '   1. Reintegros'
		Escribir '   2. Turismo'
		Escribir '   3. Descuentos'
		Escribir '   4. Estimador de Cuota AFILIADO'
		Escribir '   5. Afiliaciones ' // LISTO 
		Escribir '   6. Salir' // LISTO
		// ingresar una opci�n
		Eleccion <- '     Elija una opci�n (1-6): ' // LISTO
		centrar(Eleccion,20) // LISTO
		Leer OP
		// procesar esa opci�n
		Si (OP=1) Entonces
			// regista el paso por reintegros
			pasoPorReintegros <- Verdadero
			// seccion para reintegros
			// REINTEGROS DE LA Obra social 
			// reintegra gastos m�dicos 
			// Pr�cticas m�dicas (estimo 50 MOD )
			// Odontolog�a (estimo 100 MOD  del bono de $310)
			// Ortopedia (estimo 80 MOD )
			// .
			Repetir
				// mostrar menu de reintegros
				Limpiar Pantalla // OP variable que identifica la OPcion del usuario
				Encabezado <- 'Men� de  REINTEGROS' // REINTEGROS
				centrar(Encabezado,40)
				Escribir '   1. Pr�cticas M�dicas'
				Escribir '   2. Odontolog�a'
				Escribir '   3. Ortoped�a'
				Escribir '   4. REGRESAR'
				// ingresar una opci�n
				Eleccion <- '     Elija una opci�n (1-4): '
				centrar(Eleccion,20)
				Escribir ''
				Escribir ' Le recordamos su N�mero de afiliado: ', afiliado
				Leer OpReintegros
				// procesar esa opci�n
				Si (OpReintegros=1) Entonces
					// seccion para Practicas Medicas
					porcentajeReintegro <- 0.5
					Escribir 'INGRESE CANTIDAD DE PRACTICAS'
					Leer practicas
					Para vueltas<-1 Hasta practicas Con Paso 1 Hacer
						Escribir 'Ingrese el valor de la pr�ctica ', vueltas // se define el porcentaje de reintegro en una variable, si hay que modificarlo se hace aqui
						Leer reintegro
						montoReintegro <- montoReintegro+(reintegro*porcentajeReintegro)
						Escribir '' // ciclo PARA que de acuerdo a la cantidad de reintegros va calculando el total a reintegrar
						Escribir '             Se le reintegrar�n: $ ', (reintegro*porcentajeReintegro), ' del valor de su pr�ctica. 'Sin Saltar
						Escribir ' '
					FinPara // acumula el 50 MOD  del valor ingresado
					Escribir 'Usted ingreso ', practicas, ' pr�cticas para reintegros '
					Escribir ' La suma de sus reintegros es de: $ ', montoReintegro // muestra el importe a reintegrar
				FinSi
				Si (OpReintegros=2) Entonces
					// Seccion para Odontologia
					valorReintegroOdontologico <- 310
					Escribir '' // Informa el el importe total a reintegrarle al afiliado
					Escribir ' Le recordamos que reintegramos el 100% del bono de atenci�n Odontol�gico, valor actual: $310. '
					Escribir '    ***      No hay l�mites de bonos, ni por afiliado, grupo faminiar o mensual  ***'
					Escribir '' // valor actual de reintegro por bono presentado
					Escribir 'INGRESE CANTIDAD DE BONOS ODONTOLOGICOS A REINTEGRAR'
					Leer odontologicos
					montoOdontologico <- odontologicos*310
					Escribir 'Usted ingreso ', odontologicos, ' bonos para reintegros '
					Escribir ' El monto a reintegrar es de: $ ', montoOdontologico
				FinSi
				Si (OpReintegros=3) Entonces // multiplica la cantidad de bonos por el importe fijado para reintegro
					// Seccion para Ortopedia
					valorReintegroortopedia <- 0.8
					Escribir '' // Informa el el importe total a reintegrarle al afiliado
					Escribir '          Le recordamos que reintegramos el 80% de cada factura. '
					Escribir '*** No hay l�mites de facturas, ni por afiliado, grupo familiar o mensual  ***'
					Escribir '' // valor actual de reintegro por bono presentado
					Escribir 'INGRESE CANTIDAD DE FACTURAS A REINTEGRAR'
					Leer facturasOrtoped
					Para vueltas<-1 Hasta facturasOrtoped Con Paso 1 Hacer
						Escribir 'Ingrese el valor de la factura: ', vueltas
						Leer facturaValor
						montoOrtoped <- montoOrtoped+(facturaValor*valorReintegroortopedia)
						Escribir '' // ciclo PARA que de acuerdo a la cantidad de reintegros va calculando el total a reintegrar
						Escribir '             Se le reintegrar�n: $ ', (facturaValor*valorReintegroortopedia), ' del valor de su factura. 'Sin Saltar
						Escribir ' '
					FinPara // acumula el 80 MOD  del valor ingresado
					Escribir 'Usted ingreso ', facturasOrtoped, ' facturas para reintegro. '
					Escribir ' El monto a reintegrar es de: $ ', montoOrtoped // muestra el importe a reintegrar
				FinSi
				Si (OpReintegros>4) O (OpReintegros=0) Entonces
					Eleccion <- 'Opci�n no v�lida'
					centrar(Eleccion,30) // Informa el el importe total a reintegrarle al afiliado
					Eleccion <- ' RECUERDE QUE SOLO PUEDE ELEGIR DEL 1 AL 4 '
					centrar(Eleccion,30) // Cuando el Usuario oprime cualquier tecla dirferente a las enumeradas en el menu
				FinSi
				montoTotalReintegros <- montoReintegro+montoOrtoped+montoOdontologico
				Escribir ' Su �ltima opci�n elegida fue: ', OpReintegros
				Escribir ''
				Escribir ' El monto total obtenido en conceptos de reintegros fue de: $ ', montoTotalReintegros
				Escribir ' Presione enter para continuar' // sumo todos los reintegros que pudo haber solicitado
				Escribir ''
				Escribir 'Su n�mero de afiliado es: ', afiliado
				Esperar Tecla
			Hasta Que OpReintegros=4
		FinSi
		Si (OP=2) Entonces
			// registra el paso por Turismo
			pasoPorTurismo <- Verdadero
			// Presenta las diferentes opciones para turismo 
			// Hoteler�a 
			// alojamientos tur�sticos
			// Salidas Grupales
			// Miniturismo
			// Listado de los principales destinos que se suelen elejir. 
			contador <- 1
			Repetir
				// mostrar menu
				Limpiar Pantalla // Seccion para Turismo
				Encabezado <- 'Men� de TURISMO'
				centrar(Encabezado,40)
				Escribir '   1. Hoteleria.'
				Escribir '   2. Informaci�n para comprender los datos sobre categorias en alojamiento.'
				Escribir '   3. Salidas Grupales.'
				Escribir '   4. Mini Turismo.'
				Escribir '   5. Listado de los principales destinos que se suelen elegir. '
				Escribir '   6. REGRESAR.'
				// ingresar una opci�n
				Eleccion <- '     Elija una opci�n (1-6): '
				centrar(Eleccion,20)
				Leer OPturismo
				// procesar esa opci�n
				Si (OPturismo='1') Entonces
					// seccion para turismo hotelero
					seccion <- ' Hoteleria '
					HistorialCarga(Historia,contador,seccion)
					contador <- contador+1
					Limpiar Pantalla
					Escribir 'Estos son los hoteles que disponemos en estos momentos para usted: ' // para que la funcion de datos de contacto lo informe
					Escribir ' '
					Escribir '1.  Hotel Tronador ****, Habitaci�n por noche con desayuno. Single Junior $16.000. Mar del Plata (Bs.As.).'
					Escribir '2.  Hotel Savoia ***, Habitaci�n por noche con desayuno. Single $9.0000. San Clemente (Bs.As.).'
					Escribir '3.  Hotel Valle del Sol ***, Habitaci�n por noche con desayuno. Single $13.600. Merlo (San Luis). '
					Escribir '4.  Caba�as Calma Chicha, Tarifa por noche en: Caba�a de 2 a 3 pax: $15.000. Sierras de los Padres (Bs.As.). '
					Escribir '5.  Hotel Camaro I, Habitaci�n por noche con desayuno. Single $12.800. CABA. '
					Escribir '6.  Hotel Camaro II, Habitaci�n por noche con desayuno. Single $17.200. CABA. '
					Escribir ' '
					Escribir ' Que opci�n prefiere para que un especialista de TURISMO se comunique con usted: '
					Escribir ' Si oprime cualquier otra tecla se cancelara la posible reserva y regresara al men� anterior. '
					Leer TurismoOpcion
					Escribir ' '
					Escribir ' '
					Seg�n TurismoOpcion Hacer
						1:
							reservaGasto <- 16000
							destino <- ' Hotel Tronador ****, en Mar del Plata (Bs.As.).'
						2:
							reservaGasto <- 9000
							destino <- ' Hotel Savoia ***, en San Clemente (Bs.As.).'
						3:
							reservaGasto <- 13600
							destino <- ' Hotel Valle del Sol ***, en Merlo (San Luis).'
						4:
							reservaGasto <- 15000
							destino <- ' Caba�as Calma Chicha, en Sierras de los Padres (Bs.As.).'
						5:
							reservaGasto <- 12800
							destino <- ' Hotel Camaro I, en CABA.'
						6:
							reservaGasto <- 17200
							destino <- ' Hotel Camaro II, en  CABA.'
						De Otro Modo:
							destino <- '    ANULACION DE RESERVA ... '
							Escribir ' al elegir una opci�n distinta a las listadas su reserva se cancela.'
							reservaGasto <- 0
					FinSeg�n
					Escribir ' Su elecci�n fue: ', destino
					Escribir ' '
					Escribir ' Su gasto aproximado ser�a de $: ', reservaGasto, ' este depender� de sus datos finales cuando confirme la reserva.'
					Escribir ' '
					Escribir ' La confirmaci�n se dara luego de que un agente del �rea de Turismo se comunique con usted.'
					Escribir '     *** SI USTED CANCELO POR ERROR, PUEDE COMUNICARSE CON TURISMO Y CON GUSTO LO AYUDAREMOS ***. '
					Escribir ''
					datosTurismo(seccion)
					Escribir 'Presione enter para continuar.'
					Esperar Tecla
				FinSi
				Si (OPturismo='2') Entonces
					// "   2. Informacion para comprender los datos sobre categorias en alojamiento."
					seccion <- ' Informaci�n para comprender los datos sobre categorias en alojamiento. '
					HistorialCarga(Historia,contador,seccion)
					contador <- contador+1
					Limpiar Pantalla
					Escribir ' ' // para que la funcion de datos de contacto lo informe
					Escribir ' Cantidad de estrellas en los HOTELES: '
					Escribir ' 1 estrella: habitaci�n doble de 12 m2 m�nimo e individual de 7. Cuarto de ba�o de 3,5 m2 m�nimo. Calefacci�n y ascensor.'
					Escribir ' 2 estrellas: habitaci�n doble de 14 m2 m�nimo e individual de 7. Cuarto de ba�o de 3,5 a 4 m2 m�nimo. Tel�fono en la habitaci�n, calefacci�n, ascensor y caja de seguridad.'
					Escribir ' 3 estrellas: habitaci�n doble de 15 m2 m�nimo e individual de 8. Cuarto de ba�o de 4 m2 m�nimo. Tel�fono en la habitaci�n, calefacci�n, aire acondicionado en zonas comunes, ascensor y caja de seguridad.'
					Escribir ' 4 estrellas: habitaci�n doble de 16 m2 m�nimo e individual de 9. Cuarto de ba�o con ba�o y ducha de 4,5 m2 m�nimo. Tel�fono en la habitaci�n, calefacci�n, aire acondicionado en la habitaci�n, ascensor, bar y caja de seguridad en la habitaci�n.'
					Escribir ' 5 estrellas: habitaci�n doble de 17 m2 m�nimo e individual de 10. Cuarto de ba�o con ba�o y ducha de 5 m2 m�nimo. Tel�fono en la habitaci�n, calefacci�n, aire acondicionado en la habitaci�n, ascensor, bar y caja de seguridad en la habitaci�n.'
					Escribir ' '
					Escribir ' Alojamientos urbanos: Son todos aquellos hoteles o apartamentos que se encuentran dentro del n�cleo urbano o metropolitano.'
					Escribir ''
					Escribir ' Albergues: Son establecimientos donde hay habitaciones con varios n�meros de camas. '
					Escribir ''
					Escribir ''
					Escribir ' Business Hotel: Estos alojamientos est�n destinados, en exclusiva, a atraer mujeres y hombres de negocios.  '
					Escribir ''
					Escribir ' Capsule Hotel: Esta nueva tendencia de alojamientos a�n no se encuentra en muchas ciudades. '
					Escribir ''
					Escribir ' Bed & Breakfast: Son alojamientos que incluyen el desayuno en el precio de las habitaciones.  '
					Escribir ''
					Escribir ' Hostal o Pensi�n: Son m�s econ�micos que un hotel convencional. Ofrecen, adem�s del desayuno, la comida, cena o ambas (media pensi�n y pensi�n completa).'
					Escribir ''
					Escribir ' Casas Rurales y Hoteles R�sticos: Se encuentran en zonas alejadas de las ciudades, normalmente en pueblos peque�os y zonas de monta�a. '
					Escribir ''
					Escribir ' Campings: Los campings suelen estar formados por una gran cantidad de parcelas donde auto-caravanas u otros veh�culos establecen sus tiendas de campa�a o carpas. '
					Escribir ''
					Escribir ' Resorts y Hoteles de playa: Los resorts son grandes complejos vacacionales situados cerca de playas u otros entornos tropicales. '
					Escribir ''
					Escribir ' puede visitar la p�gina oficial del Sistema de Informaci�n Tur�stica Nacional'
					Escribir ' all� encontrar� datos oficiales de todos los alojamientos registrados en nuestro pa�s'
					Escribir '                       https://datos.yvera.gob.ar/ '
					Escribir ''
					destino <- ' Informaci�n de categorias y tipos de alojamientos '
					Escribir ' Su elecci�n fue: ', destino
					Escribir ' '
					Escribir ' '
					Escribir ' La confirmaci�n se dara luego de que un agente del �rea de Turismo se comunique con usted.'
					Escribir '     *** SI USTED CANCELO POR ERROR, PUEDE COMUNICARSE CON TURISMO Y CON GUSTO LO AYUDAREMOS ***. '
					Escribir ''
					datosTurismo(seccion)
					Escribir 'Presione enter para continuar.'
					Esperar Tecla
				FinSi
				Si (OPturismo='3') Entonces
					seccion <- ' Salidas Grupales '
					HistorialCarga(Historia,contador,seccion)
					contador <- contador+1
					Limpiar Pantalla // Secci�n SALIDAS GRUPALES
					Escribir 'Estos son las principales salidas grupales por las que podra optar.' // para que la funcion de datos de contacto lo informe
					Escribir ''
					Escribir ' 29/Octubre-01/Noviembre: Caravana `Los amantes del vino`, aloj�ndose dentro de la Bodega Algod�n Wine Estates Resort, en San Rafael, Mendoza.'
					Escribir ''
					Escribir ' 15/Noviembre: Cataratas del Iguaz� con Luna LLena.'
					Escribir ''
					Escribir ' 30/Diciembre: Fin de A�o en el Fin del Mundo, si en Ushuaia.'
					Escribir ''
					Escribir ' Ballenas y ping�inos en Puerto Madryn.'
					Escribir ''
					Escribir ' Bariloche y catedrales de hielo.'
					Escribir ''
					Escribir ' Ushuaia + El Calafate + El Chalt�n.'
					Escribir ''
					Escribir ' Esteros del Iber� + Saltos del Mocon�.'
					Escribir ''
					Escribir ' Noroeste Argentino: Tucum�n + Salta + Jujuy.'
					Escribir ''
					Escribir ' Mendoza: los caminos del vino & las olivas'
					Escribir ''
					Escribir ' San Ju�n + La Rioja + Catamarca'
					Escribir ''
					Escribir ' Buenos Aires Espectacular'
					Escribir ''
					destino <- 'Principales Salidas Grupales Nacionales'
					Escribir ' Su elecci�n fue: ', destino
					Escribir ' '
					Escribir ' '
					Escribir ' La confirmaci�n se dara luego de que un agente del �rea de Turismo se comunique con usted.'
					Escribir '     *** SI USTED CANCELO POR ERROR, PUEDE COMUNICARSE CON TURISMO Y CON GUSTO LO AYUDAREMOS ***. '
					Escribir ''
					Escribir ' Recuerde su n�mero de afiliado:  ', afiliado
					Escribir ''
					datosTurismo(seccion)
					Escribir 'Presione enter para continuar.'
					Esperar Tecla
				FinSi
				Si (OPturismo='4') Entonces
					// "  4. Mini Turismo"
					seccion <- ' Mini Turismo '
					HistorialCarga(Historia,contador,seccion)
					contador <- contador+1
					Limpiar Pantalla
					Escribir 'Le presentaremos opciones para MINI Tursimo.' // para que la funcion de datos de contacto lo informe
					Escribir '    OPRIMA UNA TECLA.   '
					Esperar Tecla
					Limpiar Pantalla
					Escribir '          Estas son las opciones de MINI TURISMO, por favor elija por cual quiere ser asesorado, luego'
					Escribir '  un especialista se comunicar� con usted para informarle todas las opciones.'
					Escribir '          No se preocupe que la reserva sobre el destino surgir� de lo charlado con el especialista '
					Escribir '  quien le indicar� los pasos a seguir.'
					Escribir ' '
					Escribir ' 1. Programa: Conociendo nuestros pueblos. '
					Escribir ' 2. Programa: Salidas de fines de semana.'
					Escribir ' 3. Programa: Escapadas. '
					Escribir ' 4. Programa: Salidas Culturales. '
					Escribir ' '
					Escribir ' Que opci�n prefiere para que un especialista de TURISMO se comunique con usted: '
					Escribir ' Si oprime cualquier otra tecla se cancelar� la posible reserva y regresar� al men� anterior. '
					Leer MiniTurismoOpcion
					Escribir ' '
					Escribir ' '
					// que tecla oprime para mini turismo
					Seg�n MiniTurismoOpcion Hacer
						1:
							Escribir ' '
							Escribir ' '
							Escribir '         Son paquetes armados conjuntamente entre nuestra �rea de Turismo y las Seccionales con el objetivo recreativo '
							Escribir '   y pedag�gico de conocer los pueblos de nuestras provincias, sus culturas y sus costumbres, en un ' // conociendo nuestros pueblos
							Escribir '   formato de salida y regreso en el d�a con servicios de bus exclusivos para nuestros Afiliados.'
							destino <- ' Conociendo nuestros pueblos.'
						2:
							Escribir ' '
							Escribir ' '
							Escribir '         Que no son otra cosa que paquetes de dos d�as y una noche, que incluyen tanto gu�as locales como coordinadores '
							Escribir '   de Turismo, contemplan las comidas y las excursiones y servicios de bus exclusivos para nuestros Afiliados.' // salidas fines de semana
							destino <- ' Salidas de fines de semana'
						3:
							Escribir ' '
							Escribir ' '
							Escribir '         Son salidas a destinos cortos, no necesariamente de nuestra provincia, que est�n previstos para ir y volver en el d�a'
							Escribir '   y pueden estar implementados mediante contrataci�n directa o a trav�s de Turismo, se lo informar� el especialista.' // escapadas
							destino <- ' Escapadas'
						4:
							Escribir ' '
							Escribir ' '
							Escribir '         Son peque�as salidas a distintos eventos culturales de la Ciudad de Buenos Aires. Ten�s la opci�n de elegir espect�culos'
							Escribir '   con descuento, gratuitos o sumarte a alg�n grupo e ir con nosotros. Se lo informar� el especialista.' // salidas culturales
							destino <- ' Salidas Culturales'
						De Otro Modo:
							Escribir ' '
							Escribir ' '
							destino <- '    ANULACION DE RESERVA ... '
							Escribir ' al elegir una opci�n distinta a las listadas su reserva se cancela.'
					FinSeg�n
					Escribir ' Su elecci�n fue: ', destino
					Escribir ' '
					Escribir ' '
					Escribir ' La confirmaci�n se dar� luego de que un agente del �rea de Turismo se comunique con usted.'
					Escribir '     *** SI USTED CANCELO POR ERROR, PUEDE COMUNICARSE CON TURISMO Y CON GUSTO LO AYUDAREMOS ***. '
					Escribir ''
					datosTurismo(seccion)
					Escribir 'Presione enter para continuar.'
					Esperar Tecla
				FinSi
				Si (OPturismo='5') Entonces
					// "   5. Rancking de destinos "
					seccion <- ' Rancking de destinos de nuestros afiliados '
					HistorialCarga(Historia,contador,seccion)
					contador <- contador+1
					Limpiar Pantalla
					Escribir 'Le presentaremos nuestro rancking de destinos.' // para que la funcion de datos de contacto lo informe
					Escribir '    OPRIMA UNA TECLA.   '
					Esperar Tecla
					Limpiar Pantalla
					Escribir ' Listado de principales destinos.'
					Escribir ' '
					Escribir ' En esta secci�n no ofician las reservas, solo es para informar el rancking actual de destinos.'
					// en esta seccion el afiliado no optara por ninguna ocion, solo es informado
					Escribir ' 1.  Bariloche.'
					Escribir ' 2.  Buenos Aires.'
					Escribir ' 3.  Iguaz�.'
					Escribir ' 4.  Mendoza.'
					Escribir ' 5.  Salta.'
					Escribir ' 6.  Ushuaia.'
					Escribir ' 7.  Calafate.'
					Escribir ' 8.  C�rdoba.'
					Escribir ' 9.  Mar del Plata.'
					Escribir ' 10. Orlando (Disney) como �nico destino internacional (nuestra recomendaci�n CONOZCAMOS NUESTRO PAIS PRIMERO).'
					Escribir ' '
					Escribir ' '
					Escribir '     *** esta secci�n es solo informativa, pero PUEDE COMUNICARSE CON TURISMO Y CON GUSTO LO AYUDAREMOS ***. '
					Escribir ''
					datosTurismo(seccion)
					Escribir 'Presione enter para continuar.'
					Esperar Tecla
				FinSi
				Si (OPturismo>'6') O (OPturismo<='0') Entonces
					Limpiar Pantalla
					Eleccion <- 'Opci�n no v�lida'
					centrar(Eleccion,30)
					Eleccion <- ' RECUERDE QUE SOLO PUEDE ELEGIR DEL 1 AL 6 ' // Cuando el Usuario oprime cualquier tecla dirferente a las enumeradas en el menu
					centrar(Eleccion,30)
				FinSi
				Escribir '  '
				Escribir 'Presione enter para continuar'
				Esperar Tecla
			Hasta Que OPturismo='6'
			// Aqui antes de regresar mostramos todo lo que eligio el afiliado
			HistorialMuestra(Historia)
			seccion <- ' REGRESANDO'
			datosTurismo(seccion)
			Escribir 'Presione enter para continuar.'
			Esperar Tecla
		FinSi
		Si (OP=3) Entonces
			// registra el paso por Descuentos
			pasoPorDescuentos <- Verdadero
			Escribir ''
			Escribir ' Le recordamos su Numero de afiliado: ', afiliado
			Escribir '' // DESCUENTOS
			Escribir ' ____AQUI LES PRESENTAMOS LOS PRINCIPALES CONVENIOS DE DESCUENTOS PARA NUESTROS AFILIADOS_______'
			Escribir ''
			Escribir 'Especial cumplea�os: 40% de descuento en comercios adheridos del lunes 11 al 30 de septiembre, con tope de $ 3.000. Para nuestros afiliados que cumplan a�os durante este mes (incluye a los integrantes del grupo familiar directo). Participan comercios de todos los rubros.'
			Escribir ''
			Escribir 'Carnicer�as y pescader�as: 35% de descuento en comercios adheridos los s�bados y domingos. Tope de $ 4.500 por Afiliado y por semana'
			Escribir ''
			Escribir 'Verduler�as y fruter�as: 40% de descuento en comercios adheridos los s�bados y domingos. Tope de $2.500 por Afiliado y por semana.'
			Escribir ''
			Escribir 'Comercios de barrio: 30% de descuento los d�as mi�rcoles y jueves de septiembre, con tope de $ 2.500 por Afiliado y por semana.'
			Escribir ''
			Escribir 'Ferias y mercados: 40% de descuento en compras realizadas en negocios adheridos, vigente todos los d�as del mes, con tope de $ 2.500 por afiliado y por semana.'
			Escribir ''
			Escribir 'Supermercados: 20% de descuento los lunes y martes de septiembre, con tope de $ 2.000 por afiliado y por semana.'
			Escribir ''
			Escribir 'J�venes de 13 a 17 a�os: $ 2.000 de regalo por persona por mes en las librer�as y kioscos adheridos para afiliados o integrantes del grupo familiar directo.'
			Escribir ''
			Escribir '10% de descuento en recargas de tarjetas SUBE en los kioscos adheridos, para afiliado titular o familiar directo.'
			Escribir ''
			Escribir 'En todos los casos se debe presentar el carnet de afiliado y DNI para acreditar la correspondencia de alguno de los beneficios aqu� descriptos.'
			Escribir ''
			Escribir 'Ante cualquier duda o consulta puede comunicarse con el delegado, referente que lo afili� o en nuestras oficinas.'
		FinSi
		Si (OP=4) Entonces
			// registra el paso por Estimador de Cuota
			pasoPorEstimadorCuota <- Verdadero
			// le muestra los valores de cuota 
			// le mostraria valores de acuerdo a con quien se afilia 
			// Le consultaria el grupo familiar y le estimaria la cuota
			Repetir
				// mostrar menu de ESTIMADOR DE CUOTA
				Limpiar Pantalla
				Encabezado <- 'Men� de ESTIMADOR DE CUOTA' // Estimador de Cuota AFILIADO
				centrar(Encabezado,40)
				Escribir '   1. VALORES POR TIPO DE AFILIADO'
				Escribir '   2. VALOR POR CONYUGE'
				Escribir '   3. VALOR POR HIJOS'
				Escribir '   4. QUIERE ESTIMAR SU CUOTA? '
				Escribir '   5. REGRESAR'
				Escribir ''
				Escribir ' Le recordamos su N�mero de afiliado: ', afiliado
				Escribir ''
				// ingresar una opci�n
				Eleccion <- '     Elija una opci�n (1-5): '
				centrar(Eleccion,20)
				Leer estimador
				// procesar esa opci�n
				Si (estimador=1) Entonces
					// seccion para MOSTRAR VALOR CUOTA SOCIAL
					Escribir ' EN ESTA SECCION LE MOSTRAREMOS LOS VALORES DE LAS CUOTAS DE ACUERDO A DETERMINADOS ASPECTOS '
					Escribir '   EL VALOR FINAL ESTAR LIGADO A LA CONFIGURACION DE SU GRUPO FAMILIAR QUE AFILIARA'
					Escribir ''
					Escribir ''
					Escribir ' valor para afiliado PLENO:      $ ', afiliadoPleno
					Escribir ' valor para afiliado ADHERENTE:  $ ', afiliadoAdherente
					Escribir ''
					Escribir ''
					Escribir ''
					Escribir ' OPRIMA UNA TECLA PARA CONTINUAR '
					Esperar Tecla
				FinSi
				Si (estimador=2) Entonces
					// Seccion para  VALOR POR CONYUGE
					Escribir ' EN ESTA SECCION LE MOSTRAREMOS LOS VALORES DE LAS CUOTAS DE ACUERDO A DETERMINADOS ASPECTOS '
					Escribir '   EL VALOR FINAL ESTARA LIGADO A LA CONFIGURACION DE SU GRUPO FAMILIAR QUE AFILIARA'
					Escribir ''
					Escribir ''
					Escribir ' valor adicional por CONYUGE:    $ ', adicionalConyuge
					Escribir ''
					Escribir ' OPRIMA UNA TECLA PARA CONTINUAR '
					Esperar Tecla
				FinSi
				Si (estimador=3) Entonces
					// Seccion para VALOR POR HIJOS
					Escribir ' EN ESTA SECCION LE MOSTRAREMOS LOS VALORES DE LAS CUOTAS DE ACUERDO A DETERMINADOS ASPECTOS '
					Escribir '   EL VALOR FINAL ESTARA LIGADO A LA CONFIGURACION DE SU GRUPO FAMILIAR QUE AFILIARA'
					Escribir ''
					Escribir ''
					Escribir ' valor adicional por HIJO:       $ ', adicionalHijos
					Escribir ''
					Escribir ' Para el caso de los hijos, no es relevante la edad ya que los mismos contar�n con cobertura sin importar su edad mientras formen parte com afiliados.'
					Escribir ''
					Escribir ''
					Escribir ' OPRIMA UNA TECLA PARA CONTINUAR '
					Esperar Tecla
				FinSi
				Si (estimador=4) Entonces
					// Seccion para QUIERE ESTIMAR SU CUOTA
					valorFinalCuota <- 0
					Escribir ''
					Escribir ' Le recordamos que EL VALOR FINAL ESTARA LIGADO A LA CONFIGURACION DE SU GRUPO FAMILIAR QUE AFILIARA '
					Escribir ''
					Escribir '' // inicializar estimador final
					Escribir 'INGRESE Tipo de Afiliado:'
					Escribir ''
					Escribir ' 1: PLENO         2: ADHERENTE '
					Escribir ''
					Escribir ' Si oprime otra tecla se cancela el proceso actual. '
					Leer tipoAfiliado
					Si tipoAfiliado=1 Entonces
						valorFinalCuota <- valorFinalCuota+afiliadoPleno
					SiNo
						Si tipoAfiliado=2 Entonces
							valorFinalCuota <- valorFinalCuota+afiliadoAdherente
						SiNo
							estimador <- 0
						FinSi
					FinSi
					// sumando al grupo de afiliado en cuestion
					Escribir 'INGRESE si sumamos c�nyuge:'
					Escribir '' // si no eligio opcion sujerida regresa al menu
					Escribir ' 1: SI         2: NO '
					Escribir ''
					Escribir ' Si oprime otra tecla se cancela el proceso actual. '
					Leer tipoAfiliado
					Si tipoAfiliado=1 Entonces
						valorFinalCuota <- valorFinalCuota+adicionalConyuge
					SiNo
						Si tipoAfiliado<>2 Entonces
							estimador <- 0
						FinSi
					FinSi
					Escribir 'INGRESE si sumamos Hijos:'
					Escribir '' // si no eligio opcion sujerida regresa al menu
					Escribir ' 1: SI         2: NO '
					Escribir ''
					Escribir ' Si oprime otra tecla se cancela el proceso actual. '
					Leer tipoAfiliado
					Si tipoAfiliado=1 Entonces
						Escribir ''
						Escribir ' Ingrese la cantidad de hijos: '
						Leer cantidadHijos
						valorFinalCuota <- valorFinalCuota+adicionalHijos*cantidadHijos
					SiNo
						Si tipoAfiliado<>2 Entonces
							estimador <- 0
						FinSi
					FinSi
					// aqui le mostraremos el valor estimado de la cuota luego que simulo su configuracion familiar
					Escribir ' EN ESTA SECCION LE MOSTRAREMOS LOS VALORES DE LA CUOTA DE ACUERDO A LOS VALORES INGRESADOS'
					Escribir '   EL VALOR FINAL ESTARA LIGADO A LA CONFIGURACION DE SU GRUPO FAMILIAR AL MOMENTO DE SU EFECTIVA AFILIACION' // si no eligio opcion sujerida regresa al menu
					Escribir ''
					Escribir ''
					Escribir ' valor ESTIMADO DE CUOTA:      $ ', valorFinalCuota
					Escribir ''
					Escribir ''
					Escribir ' OPRIMA UNA TECLA PARA CONTINUAR '
					Esperar Tecla
				FinSi
				Si (estimador>5) O (estimador=0) Entonces
					Eleccion <- 'Opci�n no v�lida'
					centrar(Eleccion,30)
					Eleccion <- ' RECUERDE QUE SOLO PUEDE ELEGIR DEL 1 AL 5 '
					centrar(Eleccion,30) // Cuando el Usuario oprime cualquier tecla dirferente a las enumeradas en el menu
				FinSi
			Hasta Que estimador=5
		FinSi
		Si (OP=5) Entonces
			// registra el paso por Afiliaciones
			pasoPorAfiliaciones <- Verdadero
			Repetir
				// mostrar menu de ESTIMADOR DE CUOTA
				Limpiar Pantalla
				Encabezado <- '  ****   SOLICITUD DE AFILIACION ****  ' // Seccion para Afiliaciones
				centrar(Encabezado,40)
				Escribir ''
				Escribir ''
				Escribir '   1. RECORDANDO LOS VALORES DE AFILIACION'
				Escribir '   2. SIMULACION DE AFILIADO'
				Escribir ''
				Escribir '   3. REGRESAR '
				Escribir ''
				// ingresar una opci�n
				Eleccion <- '     Elija una opci�n (1-3): '
				centrar(Eleccion,20)
				Leer simulador
				continuar <- 0
				// procesar esa opci�n
				Si (simulador=1) Entonces
					// seccion para LOS VALORES DE AFILIACION 
					Limpiar Pantalla
					Escribir ' EN ESTA SECCION LE MOSTRAREMOS LOS VALORES DE LAS CUOTAS DE ACUERDO A DETERMINADOS ASPECTOS '
					Escribir '' // variable que controla eleccion correcta en la afiliacion
					Escribir ' valor para afiliado PLENO:      $ ', afiliadoPleno
					Escribir ' valor para afiliado ADHERENTE:  $ ', afiliadoAdherente
					Escribir ' valor adicional por c�nyuge:    $ ', adicionalConyuge
					Escribir ' valor adicional por cada hijo:  $ ', adicionalHijos
					Escribir ''
					Escribir ' OPRIMA UNA TECLA PARA CONTINUAR '
					continuar <- 1
					Esperar Tecla
				FinSi
				Si (simulador=2) Entonces
					// Seccion para QUIERE ESTIMAR SU CUOTA
					valorFinalCuota <- 0
					continuar <- 1
					Limpiar Pantalla
					Escribir ''
					Escribir ' Le recordamos que EL VALOR FINAL ESTA LIGADO A LA CONFIGURACION DE SU GRUPO FAMILIAR.' // inicializar estimador final
					Escribir ''
					Escribir ''
					Escribir 'INGRESE Tipo de Afiliado:'
					Escribir ''
					Escribir ' 1: PLENO         2: ADHERENTE '
					Escribir ''
					Escribir ' Si oprime otra tecla se cancela el proceso actual. '
					Leer tipoAfiliado
					Si tipoAfiliado=1 Entonces
						valorFinalCuota <- valorFinalCuota+afiliadoPleno
						continuar <- 1
					SiNo
						Si tipoAfiliado=2 Entonces // comineza el calculo de la ciuota de acuerdo al tipo de afiliado
							valorFinalCuota <- valorFinalCuota+afiliadoAdherente
							continuar <- 1
						SiNo
							estimador <- 0
							continuar <- 0
						FinSi
					FinSi
					// sumando al grupo de afiliado en cuestion
					Si continuar=1 Entonces // si no eligio opcion sujerida regresa al menu
						Escribir 'INGRESE si sumamos c�nyuge:'
						Escribir ''
						Escribir ' 1: SI         2: NO '
						Escribir '' // verifica que en la consulta no elijio una opcion incorrecta al tipo de afiliado 
						Escribir ' Si oprime otra tecla se cancela el proceso actual. '
						Leer tipoAfiliado
						Si tipoAfiliado=1 Entonces
							valorFinalCuota <- valorFinalCuota+adicionalConyuge
						SiNo
							Si tipoAfiliado<>2 Entonces
								estimador <- 0
								continuar <- 0
							FinSi
						FinSi
					FinSi // si no eligio opcion sujerida regresa al menu
					Si continuar=1 Entonces
						Escribir 'INGRESE si sumamos Hijos:'
						Escribir ''
						Escribir ' 1: SI         2: NO '
						Escribir '' // verifica que en la consulta no elijio una opcion incorrecta al tipo de afiliado/suma conyuge 
						Escribir ' Si oprime otra tecla se cancela el proceso actual. '
						Leer tipoAfiliado
						Si tipoAfiliado=1 Entonces
							Escribir ''
							Escribir ' Ingrese la cantidad de hijos: '
							Leer cantidadHijos
							valorFinalCuota <- valorFinalCuota+adicionalHijos*cantidadHijos
						SiNo
							Si tipoAfiliado<>2 Entonces
								estimador <- 0
								continuar <- 0
							FinSi
						FinSi
					FinSi // si no eligio opcion sujerida regresa al menu
					Si continuar=1 Entonces
						// aqui le mostraremos el valor estimado de la cuota luego que simulo su configuracion familiar
						Escribir ' EN ESTA SECCION LE MOSTRAREMOS LOS VALORES DE LA CUOTA DE ACUERDO A LOS VALORES INGRESADOS'
						Escribir '   EL VALOR FINAL ESTARA LIGADO A LA CONFIGURACION DE SU GRUPO FAMILIAR AL MOMENTO DE SU EFECTIVA AFILIACION'
						Escribir ''
						Escribir '' // verifica que en la consulta no elijio una opcion incorrecta al tipo de afiliado/suma conyuge/suma hijos
						Escribir ' valor ESTIMADO DE CUOTA:      $ ', valorFinalCuota
						Escribir ''
						AfiliadoTempora <- CalcularNumAfiliado()
						Escribir 'SU NUMERO DE AFILIADO PROVISORIO ES :', AfiliadoTempora
						Escribir ''
						Escribir ' Ingrese un n�mero de contacto para que un especialista se comunique con usted: '
						Leer numeroTelefonico
						Escribir ''
						Escribir ' Informe este n�mero cuando se comunique usted con el �rea de AFILIACIONES o un especialista lo contacte al tel�fono facilitado'
						Escribir ''
						Escribir ' OPRIMA UNA TECLA PARA CONTINUAR '
						Esperar Tecla
					FinSi
				FinSi
				Si (continuar=0) Entonces
					Eleccion <- ' ALGUNA DE LAS OPCIONES INGRESADAS FUE INCORRECTA, POR FAVOR INTENTELO NUEVAMENTE'
					centrar(Eleccion,30)
					Eleccion <- ' RECUERDE QUE SOLO PUEDE ELEGIR DEL 1 AL 3 '
					centrar(Eleccion,30) // Cuando el Usuario oprime cualquier tecla dirferente a las enumeradas en el menu
					Esperar Tecla
				FinSi
				// Escribir "Presione enter para continuar"
				// Esperar Tecla
			Hasta Que simulador=3
		FinSi
		Si (OP>6) O (OP=0) Entonces
			Eleccion <- 'Opci�n no v�lida'
			centrar(Eleccion,30)
			Eleccion <- ' RECUERDE QUE SOLO PUEDE ELEGIR DEL 1 AL 6 '
			centrar(Eleccion,30) // Cuando el Usuario oprime cualquier tecla dirferente a las enumeradas en el menu
		FinSi
		Escribir ' Su �ltima opci�n elegida fue: ', OP
		Escribir 'Presione enter para continuar'
		// Escribir afiliado
		Esperar Tecla
	Hasta Que OP=6
	// Muestro los lugares por los que paso al menos una vez
	Si pasoPorReintegros Entonces
		Escribir ''
		Escribir 'Usted pas� al menos una vez por la secci�n REINTEGROS'
		Escribir ''
	FinSi // paso por reintegros???
	Si pasoPorTurismo Entonces
		Escribir ''
		Escribir 'Usted pas� al menos una vez por la secci�n TURISMO'
		Escribir ''
	FinSi // paso por turismo???
	Si pasoPorDescuentos Entonces
		Escribir ''
		Escribir 'Usted pas� al menos una vez por la secci�n DESCUENTOS'
		Escribir ''
	FinSi // paso por descuentos???
	Si pasoPorDescuentos Entonces
		Escribir ''
		Escribir 'Usted pas� al menos una vez por la secci�n ESTIMADOR DE CUOTA DE AFILIADO'
		Escribir ''
	FinSi // paso por estimador de cuota???
	Si pasoPorDescuentos Entonces
		Escribir ''
		Escribir 'Usted pas� al menos una vez por la secci�n AFILIACIONES'
		Escribir ''
	FinSi // paso por afiliaciones???
	Escribir ''
	Escribir ''
	Escribir ' Le agradecemos que haya visitado nuestra primer versi�n de �sta aplicaci�n tendiente a mejorar nuestra relaci�n con los actuales y futuros afiliados.'
	Escribir ''
	Escribir ''
	muchasGracias <- 'MUCHAS GRACIAS, VUELVA PRONTO'
	centrar(muchasGracias,40)
FinAlgoritmo
