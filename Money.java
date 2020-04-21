/**
 * 
 */
package main;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * This class runs the program.
 *
 * @author vanduong
 */
public class Money {
	
	
	/** The cus 2 main. */
	protected volatile static BlockingQueue<Message> cus2main = new LinkedBlockingQueue<Message>();
	
	/** The main 2 cus. */
	protected volatile static BlockingQueue<Message> main2cus = new LinkedBlockingQueue<Message>();
	
	/** The main 2 bank. */
	protected volatile static BlockingQueue<Message> main2bank = new LinkedBlockingQueue<Message>();
	
	/** The bank 2 main. */
	protected volatile static BlockingQueue<Message> bank2main = new LinkedBlockingQueue<Message>();
	
	/** The num cus alive. */
	//keep track of number of live customer thread;
	protected static volatile int numCusAlive;
	
	/** The num alive. */
	//keep track of number of live threads
	private static volatile int numAlive;
	
	/** The cus amount map. */
	private static Map<String, Integer> cusAmountMap;
	
	/** The bank amount map. */
	private static Map<String, Integer> bankAmountMap;
	
	private static Map<String, Thread> threadMap;
	

	/**
	 * The main method.
	 *
	 * @param args the arguments
	 * @throws FileNotFoundException the file not found exception
	 * @throws InterruptedException the interrupted exception
	 */
	public static void main(String[] args) throws FileNotFoundException, InterruptedException {
		System.out.println("** Banks and financial resources **");
		//extract bank info and create bank threads
		threadMap = new HashMap<String, Thread>();
		bankAmountMap = readFile("banks.txt");
		Map<String, Bank> bankInstanceMap = createBanks(bankAmountMap);
		
		System.out.println("** Customers and loan objectives **");
		cusAmountMap = readFile("customers.txt");
		Map<String, Customer> cusInstance = createCustomers(cusAmountMap, bankInstanceMap);
		//listen to requests and responses
		do {
			getCusMessage();
//			synchronized(Money.class) {
//				System.out.println("after get cus msg");
//				printQueues();
//			}
			 
			getBankMessage();
//			synchronized(Money.class) {
//				System.out.println("Afer get bank msg");
//				printQueues();
//			}
			
//			if(getNumAlive() == 0)
//				break;
			Thread.sleep(100);
		}while(getNumAlive() != 0);
		System.out.print("DONE");
//		printQueues();

	}
	
	
	/**
	 * Prints the queues.
	 */
	private static void printQueues() {
		for(Message m: cus2main) {
			System.out.println("cus2main: " + "[" + m.label + m.bank + m.customer+ "]");
		}
		for(Message m: main2cus) {
			System.out.println("main2cus: " + "[" + m.label + m.bank + m.customer+ "]");
		}
		
		for(Message m: bank2main) {
			System.out.println("bank2main: " + "[" + m.label + m.bank + m.customer+ "]");
		}
		
		for(Message m: main2bank) {
			System.out.println("main2bank: " + "[" + m.label + m.bank + m.customer+ "]");
		}
	}

	/**
	 * Gets the bank message.
	 *
	 * @return the bank message
	 * @throws InterruptedException the interrupted exception
	 */
	private static void getBankMessage() throws InterruptedException {
		Message message = bank2main.poll(4000, TimeUnit.MILLISECONDS);
//		Message message = bank2main.take();
//		if(message == null) {
//			isDone = true;
//			return;
//		}
//		Message message = bank2main.take();
		if(message == null) {
			return;
		}
		switch(message.label) {
		case "ok":
			//forward message to customer
			System.out.println(message.bank + " approves a loan of " + message.amount +
								" dollars from " + message.customer);
			main2cus.put(message);
			break;
		case "no":
			//forward message to customer
			System.out.println(message.bank + " denies a loan of " + message.amount + 
								" dollars from " + message.customer);
			main2cus.put(message);
			break;
		case "done":
			//bank is done, decrement numAlive
			System.out.println(message.bank + " has " + message.amount + " dollar(s) remaining.");
//			numAlive--;
			decrementNumAlive();
			threadMap.get(message.bank).join();
			break;
		}
		Thread.sleep(100);
		
	}

	/**
	 * Gets the cus message.
	 *
	 * @return the cus message
	 * @throws InterruptedException the interrupted exception
	 */
	private static void getCusMessage() throws InterruptedException {
		Message message = cus2main.poll(4000, TimeUnit.MILLISECONDS);
//		while(message == null) {
//			return;//customers are done
//		}
		if(getNumCusAlive() == 0 && message == null) {
			for(Map.Entry<String, Integer> entry : bankAmountMap.entrySet()) {
				main2bank.put(new Message("shut", null, entry.getKey(), 0));
			}
			return;
		}if(message == null)
			return;
		switch(message.label) {
		case "ask":
			//forward message to bank
			System.out.println( message.customer + " requests a loan of " +
								message.amount + " dollar(s) from " + message.bank);
			main2bank.put(message);
			break;
		case "satisfied":
			//customer is done, decrease numCusAlive
			System.out.println(message.customer + " has reached the objective of " +
								 + cusAmountMap.get(message.customer) + " dollar(s). Yay!");
//			numCusAlive--;
//			numAlive--;
			decrementNumCusAlive();
			decrementNumAlive();
			break;
		case "unsatisfied":
			//calculate amount customer borrowed
			int loan = cusAmountMap.get(message.customer) - message.amount;
			System.out.println(message.customer + " was only able to borrow " +
								loan + " dollar(s). Oh No!");
//			numCusAlive--;
//			numAlive--;
			decrementNumCusAlive();
			decrementNumAlive();
			break;
		}
		Thread.sleep(100);
		
	}

	/**
	 * Creates the customers.
	 *
	 * @param cusAmountMap the cus amount map
	 * @param bankInstanceMap the bank instance map
	 * @return the map
	 */
	private static Map<String, Customer> createCustomers(Map<String, Integer> cusAmountMap, Map<String, Bank> bankInstanceMap) {
		Map<String, Customer> map = new HashMap<String, Customer>();
		List<String> list = new ArrayList<String>();
		for(Map.Entry<String, Bank> entry : bankInstanceMap.entrySet()) {
			list.add(entry.getKey());
		}
		for(Map.Entry<String, Integer> entry : cusAmountMap.entrySet()) {
			Customer cus = new Customer(entry.getKey(), entry.getValue(), list);
			map.put(entry.getKey(), cus);
			Thread t = new Thread(cus);
			threadMap.put(entry.getKey(), t);
			t.start();//each threat associated with distinct instance of bank
			incrementNumCusAlive();

		}
		return map;
	}

	/**
	 * Creates the banks.
	 *
	 * @param bankList the bank list
	 * @return the map
	 */
	private static Map<String, Bank> createBanks(Map<String, Integer> bankList) {
		Map<String, Bank> map = new HashMap<String, Bank>();
		for(Map.Entry<String, Integer> entry : bankList.entrySet()) {
			Bank bank = new Bank(entry.getKey(), entry.getValue());
			map.put(entry.getKey(), bank);
			Thread t =new Thread(bank);
			threadMap.put(entry.getKey(), t);
			t.start();//each threat associated with distinct instance of bank
		}
		return map;
		
	}

	/**
	 * Read file.
	 *
	 * @param fileName the file name
	 * @return the map
	 * @throws FileNotFoundException the file not found exception
	 */
	private static Map<String, Integer> readFile(String fileName) throws FileNotFoundException {
		Map<String, Integer> map = new HashMap<String, Integer>();
		Scanner scan = new Scanner(new File(fileName));
		Pattern pattern = Pattern.compile("^(\\{)([a-z]+)(\\s*,\\s*)([0-9]+)(\\}.*)");
		while(scan.hasNextLine()) {
			String line = scan.nextLine();
			Matcher bp = pattern.matcher(line);
			if(bp.find()) {
				String name = bp.group(2);
				int total = Integer.parseInt(bp.group(4));
				System.out.println(name +": " + total);
				map.put(name, total);
				incrementNumAlive();
			}else System.out.println("Error while matching pattern");
		}
		return map;
	}

	/**
	 * Gets the num cus alive.
	 *
	 * @return the num cus alive
	 */
	public synchronized static int getNumCusAlive() {
		
		return numCusAlive;
	}
	
	/**
	 * Gets the num alive.
	 *
	 * @return the num alive
	 */
	public synchronized static int getNumAlive() {
		return numAlive;
	}
	
	/**
	 * Decrement num alive.
	 */
	public static synchronized void decrementNumAlive() {
		numAlive--;
	}
	
	/**
	 * Decrement num cus alive.
	 */
	public static synchronized void decrementNumCusAlive() {
		numCusAlive--;
	}
	
	/**
	 * Increment num alive.
	 */
	public static synchronized void incrementNumAlive() {
		numAlive++;
	}
	
	/**
	 * Increment num cus alive.
	 */
	public static synchronized void incrementNumCusAlive() {
		numCusAlive++;
	}

}
