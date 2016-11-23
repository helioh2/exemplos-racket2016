# Baseado em http://www.pyimagesearch.com/2015/09/14/ball-tracking-with-opencv/

# importar modulos necessarios
from collections import deque
import numpy as np
import argparse
import imutils
import cv2
 
# configurar e executar leitura dos argumentos
ap = argparse.ArgumentParser()
ap.add_argument("-v", "--video",
	help="path to the (optional) video file")
ap.add_argument("-b", "--buffer", type=int, default=64,
	help="max buffer size")
args = vars(ap.parse_args())


# definir limites inferiores e superiores da cor "laranja"
# no espaco de cores HSV, e depois inicializar
# a lista de pontos rastreados (que formam as linhas vermelhas)
# para descobrir os limites de uma cor diferente, utilize
# o script 'range-detector' disponivel em
# https://github.com/jrosebr1/imutils/tree/master/bin
greenLower = (0, 198, 115)
greenUpper = (255, 255, 255)
pts = deque(maxlen=args["buffer"]) #utiliza deque que eh um tipo de lista bem eficiente
 
# se nao passar o caminho para um arquivo de video, pega a referencia
# a webcam
if not args.get("video", False):
	camera = cv2.VideoCapture(0)
 
# senao, pega a referencia ao arquivo de video
else:
	camera = cv2.VideoCapture(args["video"])


# loop que fica lendo a tela:
while True:
	# pega o frame (quadro de video) atual
	(grabbed, frame) = camera.read()
 
	# se estamos vendo um video e nao pegamos nenhu8m frame
	# entao chegamos ao final do video
	if args.get("video") and not grabbed:
		break
 
	# redimensiona o frame e converte ao espaco de cor HSV
	frame = imutils.resize(frame, width=600)
	# o redimensionamento permite desempenho melhor (mais frames por segundo)
	hsv = cv2.cvtColor(frame, cv2.COLOR_BGR2HSV)
 
	# constroi uma mascara para a cor "laranja", e entao realiza
	# uma serie de dilatacoes e erosoes na imagem para remover
	# particulas (blobs) que restaram na imagem. O resultado
	# das tres linhas de codigo seguintes eh algo assim:
	# http://www.pyimagesearch.com/wp-content/uploads/2015/09/ball-tracking-mask.jpg
	mask = cv2.inRange(hsv, greenLower, greenUpper)
	mask = cv2.erode(mask, None, iterations=2)
	mask = cv2.dilate(mask, None, iterations=2)


        # encontrar contornos na mascara e inicializar o centro (x,y)
	# da bola atual
	cnts = cv2.findContours(mask.copy(), cv2.RETR_EXTERNAL,
		cv2.CHAIN_APPROX_SIMPLE)[-2]
	center = None
 
	# prossegue apenas se encontrou algum contorno
	if len(cnts) > 0:
                # encontra o contorno maior na mascara, e entao usa
                # ele para computar o menor circulo delimitador possivel
                # e o centro do circulo (centroide)
		c = max(cnts, key=cv2.contourArea)
		((x, y), radius) = cv2.minEnclosingCircle(c)
		M = cv2.moments(c)
		center = (int(M["m10"] / M["m00"]), int(M["m01"] / M["m00"]))
 
		# prossegue apenas se o raio condiz com um tamanho minimo
		if radius > 10:
			# desenha o circulo e centroide no frame,
			# e entao atualiza a lista de pontos rastreados
			cv2.circle(frame, (int(x), int(y)), int(radius),
				(0, 255, 255), 2)
			cv2.circle(frame, center, 5, (0, 0, 255), -1)
 
	# atualiza a lista de pontos
	pts.appendleft(center)


        # percorre a lista de pontos rastreados
	for i in xrange(1, len(pts)):
		# se qualquer dos pontos rastreados for None, ignore-os
		if pts[i - 1] is None or pts[i] is None:
			continue
 
		# senao, computa a espessura da linha e
		# desenha a linha que os conecta
		thickness = int(np.sqrt(args["buffer"] / float(i + 1)) * 2.5)
		cv2.line(frame, pts[i - 1], pts[i], (0, 0, 255), thickness)
 
	# mostra o frame na tela
	cv2.imshow("Frame", frame)
	key = cv2.waitKey(1) & 0xFF
 
	# se a tecla 'q' for pressionada, pare o loop
	if key == ord("q"):
		break
 
# libere a camera e feche qualquer janela aberta
camera.release()
cv2.destroyAllWindows()
