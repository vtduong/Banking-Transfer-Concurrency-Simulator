/**
 * 
 */
package main;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * The Class Customer.
 *
 * @author vanduong
 */
public class Customer implements Runnable{
	
	/** The customer. */
	private String customer;
	
	/** The total. */
	private int total;
	
	/** The bank list. */
	private List<String> bankList;
	
	/**
	 * Instantiates a new customer.
	 *
	 * @param customer the customer
	 * @param total the total
	 * @param list the list
	 */
	public Customer(String customer, int total, List<String> list) {
		super();
		this.customer = customer;
		this.total = total;
		this.bankList = list;
	}

	/* (non-Javadoc)
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {
		try {
			Thread.sleep(1000);//sleep 1000ms to make sure other threads are up
//			Money.cus2main.put(new Message("ask", customer, "rbc", 200));
//			System.out.println(customer + "received message from " + Money.main2cus.take().bank);
			borrow();
			
		} catch (InterruptedException e) {
			System.out.println("Something went wrong in customer");
			e.printStackTrace();
		}
		
	}

	/**
	 * Borrow.
	 *
	 * @throws InterruptedException the interrupted exception
	 */
	private void borrow() throws InterruptedException {
		while(!bankList.isEmpty() && total != 0) {
			//get random bank
			String bank = bankList.get(ThreadLocalRandom.current().nextInt(0, bankList.size()));	
			//get random loan
			int loan = 0;
			if(this.total > 50) {
				loan = ThreadLocalRandom.current().nextInt(1, 50 + 1);
			}else {
				loan = ThreadLocalRandom.current().nextInt(1, total + 1);
			}
			//send message to Money
			Money.cus2main.put(new Message("ask", customer, bank, loan));
			Message response = null;
			//check for response
			while(true) {
				response = Money.main2cus.take();
				if(response.customer.equalsIgnoreCase(this.customer)) {
					break;
				}else {
					Money.main2cus.put(response);//put back to the queue if not for this customer
				}
			}
			//check response detail
			if(response.label.equalsIgnoreCase("ok")) {
				//deduct total amount
				this.total -= response.amount;
			}else if(response.label.equalsIgnoreCase("no")) {
				if(bankList.contains(response.bank)) {
					//remove bank from bank list
					this.bankList.remove(bankList.indexOf(response.bank));
				}
				
			}
			Thread.sleep(ThreadLocalRandom.current().nextInt(10, 100 + 1));//sleep between loans

		}
		if(total == 0) {
			Money.cus2main.put(new Message("satisfied", customer, null, this.total));
		}else if(bankList.isEmpty()) {
			Money.cus2main.put(new Message("unsatisfied", customer, null, this.total));
		}
		
	}
	

}
