package chat;

import java.awt.TextArea;
import java.awt.TextField;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.Scanner;

public class ChatClient {
	private static final String SERVER_ADDRESS = "192.168.1.14";
	private static final int SERVER_PORT = 9090;
	
	public static void main(String[] args) {
		Scanner scanner = new Scanner(System.in);

			//1. 콘솔을 통한 닉네임 생성
			System.out.print("닉네임>>");
			String nickname = scanner.nextLine();
			
			//2. 클라이언트 실행
			new ChatWindow(nickname, SERVER_ADDRESS, SERVER_PORT).show();


	}
}
