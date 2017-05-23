import org.apache.hadoop.io.Writable;
import org.apache.hadoop.mapreduce.Reducer;
import java.util.Set;
import java.io.IOException;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.MapWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Mapper;
import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.SequenceFileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.SequenceFileOutputFormat;
import org.apache.hadoop.util.GenericOptionsParser;

import java.io.IOException;

public class Stripes {

	public static class SMWritable extends MapWritable {
		
		@Override
		public String toString() {
			StringBuilder sb = new StringBuilder();
			Set<Writable> ks = this.keySet();
			for(Object k : ks) {
				sb.append("("+k.toString()+"="+this.get(k)+")");
			}		
			return sb.toString();		
		}
	}

	public static class StripesOccurrenceMapper extends Mapper<LongWritable,Text,Text,SMWritable> {
	    private SMWritable occurrenceMap = new SMWritable();
	    private Text word = new Text();

	    @Override
	    protected void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
		//int neighbors = context.getConfiguration().getInt("neighbors", 2);
		int nb = 2;
		String[] tokens = value.toString().split("\\s+");
		if (tokens.length > 1) {
			for(int i=0; i<tokens.length; i++) {
				word.set(tokens[i]);
				occurrenceMap.clear();
				for(int j=0; j<tokens.length;j++) {
					for(int k=0; k<tokens.length;k++) {
						if(i==j || j==k || i==k)
							continue;
						StringBuilder s = new StringBuilder();
						s.append(tokens[j]);
						s.append(",");
						s.append(tokens[k]);
						Text nbs = new Text(s.toString());
						if(occurrenceMap.containsKey(nbs)) {
							IntWritable count = (IntWritable)occurrenceMap.get(nbs);
							int x = count.get() + 1;
							count.set(x);						
						}			
						else {
							occurrenceMap.put(nbs,new IntWritable(1));													
						}		
					}
					//context.write(word,occurrenceMap);
				}
				context.write(word,occurrenceMap);
	
			}
		}
	}
	}
		/*		 
			
		    for (int i = 0; i < tokens.length; i++) {
		        word.set(tokens[i]);
		        occurrenceMap.clear();
			int st = 0;
			int end = 0;			
			if(i-nb < 0)
				st = 0;
			else
				st = i - nb;
			if( i+nb >= tokens.length)
				end = tokens.length - 1;
			else
				end = i+nb;
		        //int start = (i - neighbors < 0) ? 0 : i - neighbors;
		        //int end = (i + neighbors >= tokens.length) ? tokens.length - 1 : i + neighbors;
		        for (int j = st; j <= end; j++) {
		            if (j == i) 
				continue;
		            Text nb1 = new Text(tokens[j]);
		            if(occurrenceMap.containsKey(nb1)){
		               IntWritable count = (IntWritable)occurrenceMap.get(nb1);
				int x = count.get() + 1;
		               count.set(x);
		            }
			    else{
		                occurrenceMap.put(nb1,new IntWritable(1));
		            }
		        }
		      context.write(word,occurrenceMap);
		    }
		}
	    }
	}  */

	public static class StripesReducer extends Reducer<Text, SMWritable, Text, SMWritable> {
	    private SMWritable iMap = new SMWritable();

	    @Override
	    protected void reduce(Text key, Iterable<SMWritable> values, Context context) throws IOException, InterruptedException {
		iMap.clear();
		for (SMWritable val : values) {
			Set<Writable> keys = val.keySet();
			for (Writable k : keys) {
		    		IntWritable fromCount = (IntWritable) val.get(k);
		    		if (iMap.containsKey(k)) {
		        		IntWritable count = (IntWritable) iMap.get(k);
		        		count.set(count.get() + fromCount.get());
		    		}
				else
				{
		        		iMap.put(k, fromCount);
		    		}
			}
		}
		context.write(key, iMap);
	    }

	    /*
	    private void addAll(MapWritable mapWritable) {
		Set<Writable> keys = mapWritable.keySet();
		for (Writable k : keys) {
		    IntWritable fromCount = (IntWritable) mapWritable.get(k);
		    if (incrementingMap.containsKey(k)) {
		        IntWritable count = (IntWritable) incrementingMap.get(k);
		        count.set(count.get() + fromCount.get());
		    } else {
		        incrementingMap.put(k, fromCount);
		    }
		}
	    }    */
	}

	public static void main(String[] args) throws Exception {
	    Configuration conf = new Configuration();
	    Job job = Job.getInstance(conf, "word cooccurrance");
	    job.setJarByClass(Stripes.class);
	    job.setMapperClass(StripesOccurrenceMapper.class);
	    job.setCombinerClass(StripesReducer.class);
	    job.setReducerClass(StripesReducer.class);
	    job.setOutputKeyClass(Text.class);
	    job.setOutputValueClass(SMWritable.class);
	    FileInputFormat.addInputPath(job, new Path(args[0]));
	    FileOutputFormat.setOutputPath(job, new Path(args[1]));
	    System.exit(job.waitForCompletion(true) ? 0 : 1);
	  }

}