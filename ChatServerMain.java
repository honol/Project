package chat;

import java.net.*;
import java.util.*;
import java.util.List;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import java.awt.*;
import java.io.*;

public class ChatServerMain extends JFrame {
	//1. 서버 GUI 구현
	public static JTextArea log_ta;
	
	ChatServerMain() {
		
		setTitle("Server");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		Container contentPane = getContentPane();
		contentPane.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 20));	
		
		//라벨
		JLabel log_jl = new JLabel("서버 로그 내역");
		contentPane.add(log_jl);
		
		//텍스트창 
		log_ta = new JTextArea("",17, 30);
		log_ta.setEditable(false);
		contentPane.add(new JScrollPane(log_ta));
		
		setSize(400, 400);
		setVisible(true);
	}
	private static final int PORT = 9090;

	public static void main(String[] args) {
		ServerSocket serverSocket = null;
		
		//2. 서버GUI 실행
		new ChatServerMain();
		
		//리스트에 printWriter들을 저장함으로써 broadcast메소드 사용시
		//모든 스레드의 printWriter가 data를 짝궁 클라이언트에게 전달한다.
		List<PrintWriter> listPrintWriters = new ArrayList<PrintWriter>();
		
		try {
			//1.서버소켓 생성
			serverSocket = new ServerSocket();
			

			InetAddress inetAddress = InetAddress.getLocalHost();
			String hostAddress = inetAddress.getHostAddress();
			//2.binding
			serverSocket.bind(new InetSocketAddress(hostAddress, PORT));
			// 바인딩 내용 출력
			log("bind " + hostAddress + ":" + PORT);
			
			//3. 연결 요청 기다림
			while(true) {
				Socket socket = serverSocket.accept();
				
				Thread thread = new ChatServerThread(socket, listPrintWriters);
				thread.start();
			}
			
		//에러처리
		} catch(IOException ex) {
			log("error : " + ex);
		} finally {
			if(serverSocket !=null && serverSocket.isClosed() == false) {
				try {
					serverSocket.close();
				} catch(IOException ex) {
					log("error : " + ex);
				}
			}
		}
	}
	
	//chatServer의 기록을 GUI에 출력할 static 메소드 생성
		public static void log(String log) {
			log_ta.append("[chat-server] " + log + "\n");
		}
		
}
