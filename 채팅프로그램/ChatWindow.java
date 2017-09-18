package chat;
import java.awt.BorderLayout;
import java.awt.Button;
import java.awt.Color;
import java.awt.Container;
import java.awt.Frame;
import java.awt.Panel;
import java.awt.TextArea;
import java.awt.TextField;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.charset.StandardCharsets;

import javax.swing.*;


public class ChatWindow implements ActionListener {

	private String name;
	private Container contentPane;
	private JFrame frame;
	private JPanel pannel;
	private JButton buttonSend;
	private JTextField textField;
	private JTextArea textArea;
	private PrintWriter printWriter;
	private BufferedReader bufferedReader;
	private Socket socket;
	
	//1. Server 연결
	public ChatWindow(String name, String ipAddress, int port) {
		//닉네임 저장
		this.name = name;

		
		try {
			socket = new Socket();
			socket.connect(new InetSocketAddress(ipAddress, port));
			
			if (socket != null) {
				ClientThread();
			}
			
		} catch (IOException e) {
			e.printStackTrace();
		}

	}
	
	//2. Client GUI구현
	public void show() {
		frame = new JFrame(name);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		contentPane = frame.getContentPane();
		
		pannel = new JPanel();
		buttonSend = new JButton("보내기");
		textField = new JTextField();
		textArea = new JTextArea(30, 60);

		// Button
		buttonSend.addActionListener(this);
		buttonSend.setBackground(Color.GRAY);
		buttonSend.setForeground(Color.WHITE);
		contentPane.add(buttonSend);
		
		// Textfield
		textField.setColumns(50);
		textField.addKeyListener(new KeyAdapter() {
			public void keyReleased(KeyEvent e) {
				char keyCode = e.getKeyChar();
				if (keyCode == KeyEvent.VK_ENTER) {
					actionPerformed(null);
				}
			}
		});
		contentPane.add(textField);


		// Pannel
		pannel.setBackground(Color.LIGHT_GRAY);
		pannel.add(textField);
		pannel.add(buttonSend);
		contentPane.add(BorderLayout.SOUTH, pannel);

		// TextArea
		textArea.setEditable(false);
		textArea.setBackground(Color.YELLOW);
		contentPane.add(BorderLayout.CENTER, textArea);

		frame.setVisible(true);
		frame.pack();
		
	}

	@Override
	//입력창 텍스트 처리 및 송신
	public void actionPerformed(ActionEvent e) {
		String message = textField.getText();
		
		if ("quit".equals(message) == true) {

			printWriter.println("quit");
			printWriter.flush();
		} else {
			// 9. 메시지 처리
			printWriter.println( "message:" + message );
			printWriter.flush();
			
		textField.setText("");
		textField.requestFocus();
		}
	}
	
	//3. 클라이언트 스레드
	private void ClientThread() {
		try {
			bufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream(), StandardCharsets.UTF_8));
			printWriter = new PrintWriter(new OutputStreamWriter(socket.getOutputStream(), StandardCharsets.UTF_8 ), true);
			
			//입장 메시지 송신
			printWriter.println("join:" + name);
			bufferedReader.readLine();
			printWriter.flush();
			
			Thread clientThread = new Thread(new Runnable() {
				public void run() {
					while(true) {
						try {
							String data = bufferedReader.readLine();
							if( data == null )
								break;
							textArea.append( data + "\n");
						}  catch (IOException e) {
							log("error : " + e);
						try {
							socket.close();
							break;
						} catch (IOException ex) {
							log("error : " + ex);	
						} finally {
							try {
								if (bufferedReader != null)
									bufferedReader.close();

								if (printWriter != null)
									printWriter.close();
						
								if (socket != null && socket.isClosed() == false)
									socket.close();
								
							} catch (IOException ex) {
								log("error : " + ex);
							}
						}
						}
					}
				}
		});
		
		//스레드 실행
		clientThread.start();
		
		} catch (IOException e) {
			log("error : " + e);
		}
	}
	public void log(String log) {
		textArea.append("[chat-client] " + log);
	}
}


	
		
