package logic;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Ship {
	
	private final String PREFIX = "abcdefghijk";
	
	private BufferedReader br;
	
	public Ship() {
		InputStreamReader isr = new InputStreamReader(System.in);
	    br = new BufferedReader(isr);
	}
	
	private String readLine() {
		try {
			return br.readLine();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return "";
	}
	
	public void move() {
		System.out.println(PREFIX + "_move");
	}
	
	public void take() {
		System.out.println(PREFIX + "_take");
	}
	
	public void turn() {
		turn(Direction.BACK);
	}
	
	public void turn(Direction dir) {
		switch(dir) {
		case RIGHT:
			System.out.println(PREFIX + "_turn_right");
			break;
		case LEFT:
			System.out.println(PREFIX + "_turn_left");
			break;
		case BACK:
			System.out.println(PREFIX + "_turn_back");
			break;
		default:
		    throw new RuntimeException("unknown argument");
		}
	}
	
	public void put() {
		put(Item.BUOY);
	}
	
	public void put(Item item) {
		switch(item) {
		case TREASURE:
			System.out.println(PREFIX + "_put_treasure");
			break;
		case BUOY:
			System.out.println(PREFIX + "_put_buoy");
			break;
		default:
		    throw new RuntimeException("unknown argument");
		}
			
	}
	
	public Item look() {
		return look(Direction.HERE);
	}
	
	public Item look(Direction dir) {
		switch(dir) {
		case RIGHT:
			System.out.println(PREFIX + "_?_look_right");
			break;
		case LEFT:
			System.out.println(PREFIX + "_?_look_left");
			break;
		case HERE:
			System.out.println(PREFIX + "_?_look_here");
			break;
		case BACK:
			System.out.println(PREFIX + "_?_look_back");
			break;
		case FRONT:
			System.out.println(PREFIX + "_?_look_front");
			break;
		default:
		    throw new RuntimeException("unknown argument");
		}
		String ret = readLine();
		if (ret.contains("buoy")) {
			return Item.BUOY;
		} else if (ret.contains("monster")) {
		    return Item.MONSTER;
		} else if (ret.contains("treasure")) {
		    return Item.TREASURE;
		} else if (ret.contains("wave")) {
		    return Item.WAVE;
		} else if (ret.contains("border")) {
		    return Item.BORDER;
		} else {
			return Item.NOTHING;
		}
	}
}
