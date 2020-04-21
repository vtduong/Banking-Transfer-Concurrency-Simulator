/**
 * 
 */
package main;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;

/**
 * The Class Bank.
 *
 * @author vanduong
 */
public class Bank implements Runnable{
	
	/** The total. */
	//current total amount
	private volatile int total;
	
	/** The bank. */
	//bank name
	private String bank;
	
	
	/**
	 * Instantiates a new bank.
	 *
	 * @param bank the bank
	 * @param total the total
	 */
	public Bank(String bank, int total ) {
		this.total = total;
		this.bank = bank;
		
	}

	/* (non-Javadoc)
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {
		try {
			Thread.sleep(1000);//sleep 1000ms to make sure other threads are up
//			System.out.println(bank + "received message from " + Money.main2bank.take().customer);
//			Money.bank2main.put(new Message("ok", "jill", bank, 10));
			lend();
		
		} catch (InterruptedException e) {
			System.out.println("Something went wrong in Bank");
			e.printStackTrace();
		}
		
	}

	/**
	 * Lend.
	 *
	 * @throws InterruptedException the interrupted exception
	 */
	private void lend() throws InterruptedException {
		boolean isDone = false;
		//keep the thread active as long as there are live customers
		while(true) {
			//get message from main
			Message message = null;
			while(true) {
				message = Money.main2bank.take();
//				message = Money.main2bank.poll(5000, TimeUnit.MILLISECONDS);

//				if(message == null) {//no more messages from customers
//					Money.bank2main.put(new Message("done", null, bank, this.total));
//					isDone = true;
//					break;
//				}
				if(message.bank.equalsIgnoreCase(this.bank)) {
					break;
				}else {
					//put the message back to the queue queue
					Money.main2bank.put(message);
				}
			}
//			if(isDone) {
//				break;
//			}
			//check message
			if(message.label.equalsIgnoreCase("shut")) {
				Money.bank2main.put(new Message("done", null, bank, this.total));
				break;
			}
			if(this.total < message.amount) {
				Money.bank2main.put(new Message("no", message.customer, bank, message.amount));
			}else {
				//deduct bank total
				this.total -= message.amount;
				Money.bank2main.put(new Message("ok", message.customer, bank, message.amount));
			}
			Thread.sleep(100);
		}
//		System.out.println(bank + " is DONE");
//		System.out.println("NumAlive and NumCusAlive in " + bank + " " +Money.getNumAlive() + " " + Money.getNumCusAlive() );
		
		//no more live customers, send the last message
//		Money.bank2main.put(new Message("done", null, bank, this.total));
		
	}
	
	
}
