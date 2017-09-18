package chat;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.net.*;

public class ChatServerThread extends Thread {
	private static final String DIVIDER = ":";
	private String nickname;
	private Socket socket;
	private List<PrintWriter> listPrintWriter;
	
	//1. Socket과 List를 인자로하는 생성자
	public ChatServerThread(Socket socket, List<PrintWriter> listPrintWriter) {
		//위에 생성한 참조변수를 했으므로, 인자를 해당 참조변수에 저장함
		//앞으로의 ChatServerThread가 가진 메소드에 이 두가지를 활용할려고 저장시키는듯.
		this.socket = socket;
		this.listPrintWriter = listPrintWriter;
	}
	
	//2. 서버 스레드
	public void run() {
		//읽고 쓸 버퍼드 리더와 라이터 생성
		BufferedReader bufferedReader = null;
		PrintWriter printWriter = null;
		
		try {
			
			bufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream(),StandardCharsets.UTF_8));
			printWriter = new PrintWriter(new OutputStreamWriter(socket.getOutputStream(),StandardCharsets.UTF_8),true);
			
			//서버와 연결된 클라이언트 정보 출력
			InetSocketAddress inetSocketAddress = (InetSocketAddress)socket.getRemoteSocketAddress();
			String remoteHostAddress = inetSocketAddress.getHostName();
			int remoteHostPort = inetSocketAddress.getPort();
			//메인 클래스에 있는 log메소드를 불러와서 처리.
			ChatServerMain.log("연결됨 from" + remoteHostAddress + ":" + remoteHostPort);
			
			//클라이언트로부터 온 데이터 처리
			while(true) {
				String request = bufferedReader.readLine();

				if(request == null) {
					ChatServerMain.log("클라이언트로 부터 연결 끊김");
					doQuit(printWriter);
					break;
				}
				
				// a : b 형태의 클라이언트 데이터 처리
				String[] tokens = request.split(DIVIDER);
				if ("join".equals(tokens[0]))
					doJoin(printWriter,tokens[1]);
				else if ("message".equals(tokens[0]))
					doMessage(tokens[1]);
				else if ("quit".equals(tokens[0])) {
					doQuit(printWriter);
					break;
				} else
					ChatServerMain.log("에러 : 알수 없는 요청명령(" + tokens[0] + ")");
			}
		} catch(IOException ex) {
			ChatServerMain.log("error : " + ex);
			doQuit(printWriter);
		} finally {
			try {
				bufferedReader.close();
				printWriter.close();
				if(socket.isClosed() == false) {
					socket.close();
				}
			} catch(IOException ex) {
				ChatServerMain.log("error : " + ex);
			}
		}
	}
	
	//데이터 처리하는데 사용되는 메소드 정의
	private void doJoin(PrintWriter printWriter, String nickname) {
		this.nickname = nickname;
		
		String message = nickname + "님이 입장했습니다.";
		broadcast(message);
		
		addPrintWriter(printWriter);
		
		printWriter.println("정상적인 접근");
	}
	
	private void doQuit(PrintWriter printWriter) {
		removePrintWriter(printWriter);
		String data = nickname + "님이 퇴장하였습니다.";
		broadcast(data);
	}
	
	private void doMessage(String message) {
		String data = nickname + ":" + message;
		broadcast(data);
	}
	
	private void addPrintWriter(PrintWriter printWriter) {
		synchronized(listPrintWriter) {
			listPrintWriter.add(printWriter);
		}
	}
	
	private void removePrintWriter(PrintWriter printWriter) {
		synchronized(listPrintWriter) {
			listPrintWriter.remove(printWriter);
		}
	}
	
	//3. 메시지 전달
	private void broadcast(String data) {
		//서버와 싱크를 맞춤으로써 모든 클라이언트에게 완전한 형태의 메시지 전달
		synchronized(listPrintWriter) {
			int count = listPrintWriter.size();
			for(int i=0; i<count; i++) {
				PrintWriter printWriter = listPrintWriter.get(i);
				printWriter.println(data);
				printWriter.flush();
			}
		}
	}

}
