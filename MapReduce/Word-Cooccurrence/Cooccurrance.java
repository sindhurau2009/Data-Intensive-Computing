import org.apache.hadoop.io.Text;
import org.apache.hadoop.io.Writable;
import org.apache.hadoop.io.WritableComparable;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.lang.Math;

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
public class Cooccurrance {
	public static class WordPair implements Writable,WritableComparable<WordPair> {

	    private Text word;
	    private Text neighbor;

	    public WordPair(Text word, Text neighbor) {
		this.word = word;
		this.neighbor = neighbor;
	    }

	    public WordPair(String word, String neighbor) {
		this(new Text(word),new Text(neighbor));
	    }

	    public WordPair() {
		this.word = new Text();
		this.neighbor = new Text();
	    }
		
	
	    @Override
	    public int compareTo(WordPair other) {
		int returnVal = this.word.compareTo(other.getWord());
		if(returnVal != 0){
		    return returnVal;
		}
		if(this.neighbor.toString().equals("*")){
		    return -1;
		}else if(other.getNeighbor().toString().equals("*")){
		    return 1;
		}
		return this.neighbor.compareTo(other.getNeighbor());
	    }

	    public static WordPair read(DataInput in) throws IOException {
		WordPair wordPair = new WordPair();
		wordPair.readFields(in);
		return wordPair;
	    }

	    @Override
	    public void write(DataOutput out) throws IOException {
		word.write(out);
		neighbor.write(out);
	    }

	    @Override
	    public void readFields(DataInput in) throws IOException {
		word.readFields(in);
		neighbor.readFields(in);
	    }
		
	    @Override
	    public String toString() {
		return "{word=["+word+"]"+
		       " neighbor=["+neighbor+"]}";
	    }
	
	    @Override
	    public boolean equals(Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;

		WordPair wordPair = (WordPair) o;

		if (neighbor != null ? !neighbor.equals(wordPair.neighbor) : wordPair.neighbor != null) return false;
		if (word != null ? !word.equals(wordPair.word) : wordPair.word != null) return false;

		return true;
	    }

	    @Override
	    public int hashCode() {
		int result = word != null ? word.hashCode() : 0;
		result = 163 * result + (neighbor != null ? neighbor.hashCode() : 0);
		return result;
	    }
		

	    public void setWord(String word){
		this.word.set(word);
	    }
	    public void setNeighbor(String neighbor){
		this.neighbor.set(neighbor);
	    }

	    public Text getWord() {
		return word;
	    }

	    public Text getNeighbor() {
		return neighbor;
	    }
	}

	// Pairs Mapper
	public static class PairsOccurrenceMapper extends Mapper<LongWritable, Text, WordPair, IntWritable> {
	    private WordPair wordPair = new WordPair();
	    private IntWritable ONE = new IntWritable(1);

	    @Override
	    protected void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
		//int neighbors = context.getConfiguration().getInt("neighbors", 2);
		int nb = 2;		
		String[] tokens = value.toString().split("\\s+");
		if (tokens.length > 1) {
		  for (int i = 0; i < tokens.length; i++) {
			wordPair.setWord(tokens[i]);
			int st, end;
			if( i-nb<0)
				st = 0;
			else
				st = i-nb;
			if( i+nb>=tokens.length)
				end = tokens.length-1;
			else
				end = i+nb;
		     //int start = (i - neighbors < 0) ? 0 : i - neighbors;
		     //int end = (i + neighbors >= tokens.length) ? tokens.length - 1 : i + neighbors;
		      for (int j = st; j <= end; j++) {
		          if (j == i) 
				continue;
		           wordPair.setNeighbor(tokens[j]);
		           context.write(wordPair, ONE);
		      }
		  }
	      }
	  }
	}

	//Pairs Reducer
	public static class PairsReducer extends Reducer<WordPair,IntWritable,WordPair,IntWritable> {
	    private IntWritable x = new IntWritable();
	    @Override
	    protected void reduce(WordPair key, Iterable<IntWritable> values, Context context) throws IOException, InterruptedException {
		int c = 0;
		for (IntWritable val : values) {
		     c += val.get();
		}
		x.set(c);
		context.write(key,x);
	    }
	}

  	public static void main(String[] args) throws Exception {
	    Configuration conf = new Configuration();
	    Job job = Job.getInstance(conf, "word cooccurrance");
	    job.setJarByClass(Cooccurrance.class);
	    job.setMapperClass(PairsOccurrenceMapper.class);
	    job.setCombinerClass(PairsReducer.class);
	    job.setReducerClass(PairsReducer.class);
	    job.setOutputKeyClass(WordPair.class);
	    job.setOutputValueClass(IntWritable.class);
	    FileInputFormat.addInputPath(job, new Path(args[0]));
	    FileOutputFormat.setOutputPath(job, new Path(args[1]));
	    System.exit(job.waitForCompletion(true) ? 0 : 1);
	  }

}