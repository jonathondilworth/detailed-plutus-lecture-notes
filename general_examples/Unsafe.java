import java.net.URL;
import java.net.HttpURLConnection;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.lang.StringBuilder;
import java.net.MalformedURLException;
import java.io.IOException;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

class Unsafe
{
	/**
	 * Get the HTTP repsonse body (in string format) at a spec'd URL
	 * 
	 * Usually this would be refactored, but this is a simple example
	 * In addition, there are ample comments here, so.. do one.
	 * 
	 * @param fromUrl A string: HTTP endpoint
	 * @return        The contents at the fromUrl param
	 */
	public static String getSingleLineHttpResponse(String fromUrl)
	{
		// typical Java; x aint good enough we have to use long var names
		// the code should read such that it doesn't need comments
		// ref: Uncle Bob: Clean Code
		try
		{
			URL webAddress = new URL(fromUrl);

			HttpURLConnection webConnection = (HttpURLConnection) webAddress.openConnection();

			if(webConnection.getResponseCode() != 200)
			{
				throw new IOException("Error: Sorry, but the connection was unsuccessful.");
			}

		    BufferedReader someMemReader = new BufferedReader(new InputStreamReader(webConnection.getInputStream()));

		    return someMemReader.readLine();
		}
		catch(MalformedURLException malformedException)
		{
			return "Error: the supplied URL is malformed or.. BAD";
		}
		catch(IOException inputOutputException)
		{
			return "Error: the input resource is unavailable or.. BAD";
		}
	}

	/**
	 * Simply removes an expected value from square brackets
	 * ANY Exception returns -1
	 * 
	 * @param singularJSONInput	a singular line of JSON [...]
	 * @return a value between 1 and 100 or -1 on error
	 */
	public static int extractIntFromSquareBrackets(String singularJSONInput)
	{
		try
		{
			Pattern valuesWithinAnySquareBrackets = Pattern.compile("\\[(.*?)\\]");
			Matcher matches = valuesWithinAnySquareBrackets.matcher(singularJSONInput);
			String value = matches.find() ? matches.group(1) : "-1";
			return  Integer.parseInt(value);
		}
		catch(Exception e)
		{
			return -1;
		}
	}

	/**
	 * Gimmie' a 5! (of type int)
	 * 
	 * @return 		(int) 5
	 */
	public static int doSomething() 
	{
		return 5;
	}

	/**
	 * ...
	 * 
	 * @return If a random value between (1 : 100 + 5) > 54 then True else False
	 **/
	public static boolean doSomethingComplex()
	{
		String randomFromAPI = getSingleLineHttpResponse("http://www.randomnumberapi.com/api/v1.0/randomnumber");

		int randomNumberFromAPI = extractIntFromSquareBrackets(randomFromAPI);

		return ((randomNumberFromAPI + doSomething()) > 54);
	}

	/**
	 * The imperative, unsafe (but useful) program!
	 */ 
	public static void main(String[] args)
	{
		// Let the box heat up... can we know the result? NOPE.
		System.out.println(doSomethingComplex());
	}

}
