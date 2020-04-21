/**
 * 
 */
package main;

import java.util.concurrent.locks.Condition;

/**
 * This class represents a message of communication among threads.
 *
 * @author vanduong
 */
public class Message {

	/** The customer. */
	protected String customer;
	
	/** The label. */
	protected String label;
	
	/** The bank. */
	protected String bank;
	
	/** The amount. */
	protected int amount;

	/**
	 * Instantiates a new message.
	 *
	 * @param label the label
	 * @param customer the customer
	 * @param bank2 the bank 2
	 * @param amount the amount
	 */
	public Message(String label, String customer, String bank2, int amount) {
		this.label = label;
		this.customer = customer;
		this.bank = bank2;
		this.amount = amount;
	}

}
