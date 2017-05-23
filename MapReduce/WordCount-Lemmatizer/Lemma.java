import java.io.IOException;
import java.util.StringTokenizer;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

import java.util.Collections;
import java.util.ArrayList;
import java.util.HashMap;
import java.io.*;
import java.io.FileReader;
import java.util.*;

public class Lemma {
	
	static HashMap<String,String> map = new HashMap<String, String>();
	
 	public static class TokenizerMapper extends Mapper<LongWritable, Text, Text, Text>{

  		private final static IntWritable one = new IntWritable(1);
    		private Text word = new Text();
		private Text val = new Text();
		int c = 0;

		
    		public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
        		String[] tokens = value.toString().split("\\r?\\n");
			if (tokens.length > 0) {
		    		for (int i = 0; i < tokens.length; i++) {
					String[] tk = tokens[i].toString().split(">");
					if(tk.length==2) {
						String[] tkn = tk[1].toString().split("\\s+");
						StringBuilder sb = new StringBuilder();
						sb.append(tk[0]);
						sb.append(">");
						//System.out.println(sb);
						//sb.append(tk[1]);
						//System.out.println(k);
						val.set(sb.toString());
						for(int j = 0; j< tkn.length; j++) {
							c = 0;
							//word.set(tokens[i]);
							//System.out.println(tkn[j]);
							tkn[j].replaceAll("j","i");
							tkn[j].replaceAll("v","u");
							if(map.containsKey(tkn[j]))
							{
								tkn[j] = map.get(tkn[j]);
								word.set(tkn[j]);
								context.write(word,val);
								c = 1;
								
							}
							if( c == 0) {
								word.set(tkn[j]);
							
								context.write(word,val);
							}
						}
					}
				}
			}
    		}
  	}

  	public static class IntSumReducer extends Reducer<Text,Text,Text,Text> {
    		private IntWritable result = new IntWritable();
		//private Text res = new Text();
    		public void reduce(Text key, Iterable<Text> values, Context context) throws IOException, InterruptedException {
      			/*int sum = 0;
			StringBuilder s = new StringBuilder();			
			for (Text val : values) {
				String val1 = val.toString();
       				s.append(val1);
      			} */

			int c=0;
			/* for(Text v : values)
			{
				c++;
			} */
			//System.out.println(c);
			StringBuilder s = new StringBuilder();
			int i=0;
    			for(Text val : values) {
				//System.out.println(i);
				String s1 = val.toString();
				s.append(s1);				

				/* if(i<c) {
					String s1 = val.toString();
					s1.replaceAll(">",",");
					s.append(s1);				
				}
				else {
					s.append(val.toString());	
				}
				i++; */
    			}
			//String s1 = s.toString();
			//s.substring(0,s.length()-4);
			//s.append(">");
			Text res = new Text(s.toString());
      			//result.set(sum);
      			context.write(key, res);
    		}
  	}

  	public static void main(String[] args) throws Exception {


		BufferedReader br1 = new BufferedReader(new FileReader(args[2]));
		String line1 =  null;
		while((line1=br1.readLine())!=null){
			String str[] = line1.split(",");
			map.put(str[0], str[2]);
			
		}

		/*		
		for (Map.Entry<Map<String, String>> entry: map.entrySet()) {
 			String value = entry.getKey();
   			Integer count = result.get(value);
   			if (count == null)
      				result.put(value, new Integer(1));
   			else
      				result.put(value, new Integer(count+1));
		}
		*/

    		Configuration conf = new Configuration();
    		Job job = Job.getInstance(conf, "word count");
    		job.setJarByClass(Lemma.class);
    		job.setMapperClass(TokenizerMapper.class);
		job.setCombinerClass(IntSumReducer.class);
		job.setReducerClass(IntSumReducer.class);
		job.setOutputKeyClass(Text.class);
		job.setOutputValueClass(Text.class);
		FileInputFormat.addInputPath(job, new Path(args[0]));
		//FileInputFormat.addInputPath(job, new Path(args[2]));
		FileOutputFormat.setOutputPath(job, new Path(args[1]));
		System.exit(job.waitForCompletion(true) ? 0 : 1);
  	}
}