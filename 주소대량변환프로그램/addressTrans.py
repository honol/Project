from urllib.request import urlopen
from bs4 import BeautifulSoup
from splinter import Browser
import time
import re
fout = open('일반주소록_new.csv', 'a')
fin = open('일반주소록_old.csv', 'r').readlines()
address_old = []
for i in fin:
	address_old.append(i.strip())

browser = Browser('chrome')

url = "http://www.juso.go.kr/support/AddressMainSearch.do?searchType=TOTAL"

browser.visit(url)

for i in address_old:
	create = 0
	print('<', i, '검색중>')
	browser.fill('searchKeyword', i)
	button = browser.find_by_xpath('//*[@id="searchButton"]')
	button.click()
	time.sleep(2)
	web_page = BeautifulSoup(browser.html, 'html.parser')
	#검색 결과 없을 경우 tb가 존재하지 않으므로 빈 리스트 할당
	tbl = web_page.findAll(attrs = {'class': 'tbl'})
	try :
		bool(tbl[0])
		#tb[i]에는 검색 결과가 하나씩 들어감. 몇몇 허수도 존재.
		for j in tbl:
			#tbl의 원소 하나하나인 j는 검색 결과 하나를 의미
			#cell은 한 칸의 하단의 구주소와 관련지번을 포함함
			cell = j.findAll(attrs = {'class': 'cell num'})
			for k in cell:
				try:
					k.p.decompose()
				except Exception:
					pass
				#get은 k를 수정하지 않고 반환만 한다.
				collect = k.get_text().strip().replace(' ', '').find(i[len(i)-2:len(i)])
				word = k.get_text().strip().replace(' ', '').replace('\n','')
				if collect == -1:
					pass
				else:
					#완벽히 뒷자리가 일치하는 경우, 뒤에 추가 공백이 있는경우
					if len(word)-collect-2 ==0 or word[collect+2] == ' ' or word[collect+2] == '\t' or word[collect+2] == '\n':
						address_new = j.find(attrs = {'class': 'cell st'}).get_text().strip().replace('\n', '') 
						create += 1


	except Exception:
		fout.write("검색 결과가 없습니다." + '\r')
		continue

	if create == 0:
		fout.write("일치하는 신주소가 없습니다." + '\r')
	elif create >= 1:
		fout.write(address_new + '\r')
		print("일치하는 도로명주소를 찾았습니다.")
	else:
		fout.write("검색 결과에 이상을 감지했습니다." + '\r')

fout.close()